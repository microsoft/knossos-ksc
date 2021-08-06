import asyncio
import functools
import multiprocessing as mp
import sys
import os
from collections import OrderedDict
from concurrent.futures import ProcessPoolExecutor
from time import time
from typing import Any, Callable, Dict, Optional, Sequence

import matplotlib.pyplot as plt
import numpy as np
import ray
import seaborn as sns
from absl import app, flags
from tqdm import tqdm
from queue import Queue as QueueQueue
from ray.experimental.queue import Queue as RayQueue

from rlo.astar_search import AStarSearcher, Expander
from rlo.factory import regressor_from_config
from rlo.expr_sets import get_expression_set
from rlo.expression_util import ExprWithEnv
from rlo.tf_model import ModelWrapper
from rlo.rewrites import get_rules, RuleSet

sys.path.append(os.path.dirname(__file__))
import ray_manager as rm
import sync_search as ss
from async_search import batched_astar_search, multi_rollout, continuous_astar_search
from batchers import BatchSizeBatcher
from connections import Connection, ConnectedFunction, get_connected_functions
from servers import run_model_server

flags.DEFINE_string("search", default="astar", help="astar or rollout")
flags.DEFINE_integer(
    "gnn_budget",
    default=16000,
    help="gnn budget across all starting expressions for astar search",
)
flags.DEFINE_integer(
    "num_rollouts",
    default=4,
    help="number of rollouts per starting expression for rollout search",
)
flags.DEFINE_integer("num_searches", default=8, help="number of searches to perform")

# sync / async flags
flags.DEFINE_string(
    "save_path",
    default="results.png",
    help="Location to save sync/async plot to. Uses `None` to show without saving",
)
flags.DEFINE_integer(
    "num_cpus", default=8, help="Number of CPUs to initialize ray with"
)
flags.DEFINE_boolean("skip_sync", default=False, help="Skip synchronous evaluations")

# multi_server flags
flags.DEFINE_boolean("multi_server", default=False, help="Run multi-server experiment")
flags.DEFINE_integer(
    "num_gpus",
    default=1,
    help="Number of GPUs to initialize ray with for multi_server experiment",
)
flags.DEFINE_float(
    "total_gpu_frac",
    default=0.6,
    help="Total gpu fraction to use for multi_server experiment",
)
flags.DEFINE_integer(
    "num_servers", default=2, help="number of servers to use in multi_server experiment"
)
flags.DEFINE_integer(
    "num_procs", default=2, help="number of splits to use in multi_server experiments"
)
flags.DEFINE_bool(
    "managed_queues", default=False, help="Use ray_manager.Queue as QueueFactory"
)

flags.DEFINE_boolean("continuous", default=False, help="run continuous experiment")


class Metrics(object):
    def __init__(self, name: str, group: str, tracker: "MetricTracker"):
        self._name = name
        self._group = group
        self._tracker = tracker
        self.batch_sizes = []
        self.model_times = []
        self.update_times = []
        self.run_time = 0

    @property
    def name(self):
        return self._name

    def __enter__(self):
        assert self._tracker._current is None
        self._tracker._current = self
        print(f"Running {self._name}...")
        self._prog = tqdm()
        self.start_time = time()
        return self

    def update(self, start_time: float, end_time: float, batch_size: int):
        self.update_times.append(end_time)
        self.model_times.append(end_time - start_time)
        self.batch_sizes.append(batch_size)
        self._prog.update(batch_size)

    def __exit__(self, *args, **kwargs):
        self._tracker._current = None
        self._prog.close()
        self.stop_time = time()
        with open("/tmp/data.txt", "a") as fp:
            fp.write(", ".join((str(bs) for bs in self.batch_sizes)))
            fp.write("\n")
        self.report()

    def report(self):
        if self.model_times:
            model_time = np.sum(self.model_times[1:])  # first one includes compilation
            run_time = self.update_times[-1] - self.update_times[1]  # skip first / last
        else:
            model_time = None
            run_time = self.stop_time - self.start_time
        print("################")
        print(f"Finsihed {self._name}")
        print(f"Total search time: {run_time:.2f} s")
        if model_time is not None:
            print(f"Total model time : {model_time:.2f} s")
            print(f"Fraction in model: {model_time / run_time*100:.2f} %")
        if self.batch_sizes:
            print("Batch info")
            print(f"  num_batches:  {len(self.batch_sizes)}")
            print(f"  num elements: {sum(self.batch_sizes)}")
            print(f"  size mean:    {np.mean(self.batch_sizes):.2f}")
            print(f"  size std:     {np.std(self.batch_sizes):.2f}")


class GroupScope(object):
    def __init__(self, name: str, tracker: "MetricTracker"):
        self.name = name
        self.tracker = tracker

    def __enter__(self):
        self.tracker._groups_stack.append(self)
        return self

    def __exit__(self, *args, **kwargs):
        assert self.tracker._groups_stack.pop() == self


class MetricTracker(object):
    def __init__(self):
        self._metrics = OrderedDict()
        self._current = None
        self._groups_stack = []

    def tracked(self, name: str) -> Metrics:
        group = "/".join(((g.name) for g in self._groups_stack))
        metrics = Metrics(name, group, self)
        group_dict = self._metrics.setdefault(group, OrderedDict())
        assert name not in group_dict
        group_dict[name] = metrics
        return metrics

    def group(self, name: str) -> GroupScope:
        return GroupScope(name, self)

    def wrap_eval_fn(self, eval_fn: Callable):
        def new_eval_fn(data):
            start_time = time()
            out = eval_fn(data)
            self._current.update(start_time, time(), len(out))
            return out

        return new_eval_fn

    def wrap_model(self, model: ModelWrapper):
        model._evaluate_all_time_left = self.wrap_eval_fn(model._evaluate_all_time_left)

    def do_plots(self, max_batch_size: int, max_batch_size_y: float = 0.25, skip=()):
        num_groups = len(self._metrics)
        max_time = 0
        if num_groups == 0:
            return
        _, ax = plt.subplots(2, num_groups, squeeze=False, figsize=(24, 16))

        for (ax0, ax1), (group_name, group_metrics) in zip(ax.T, self._metrics.items()):
            ax0.set_title(f"{group_name}")
            ax0.set_xlabel("Batch size")
            ax1.set_xlabel("examples evaluated")
            max_batch_size = 0
            for metrics in group_metrics.values():
                if metrics.name in skip:
                    continue
                batch_sizes = np.array(metrics.batch_sizes)
                max_batch_size = max(max_batch_size, np.max(batch_sizes))
                model_times = (
                    np.cumsum(np.array(metrics.model_times)) - metrics.model_times[0]
                )
                update_times = np.array(metrics.update_times) - metrics.update_times[0]
                max_time = max(max_time, update_times[-1])
                cum_batch_sizes = np.cumsum(batch_sizes)
                sns.distplot(batch_sizes, ax=ax0, label=metrics.name)
                sns.lineplot(
                    cum_batch_sizes, update_times, ax=ax1, label=metrics.name,
                )
                color = ax1.lines[-1].get_c()
                sns.lineplot(
                    cum_batch_sizes, model_times, ax=ax1, color=color,
                )
                ax1.lines[-1].set_linestyle("--")

        for ax0, ax1 in ax.T:
            ax0.set_ylim(0, max_batch_size_y)
            ax0.set_xlim(0, max_batch_size)
            ax1.set_ylim(0, max_time)
        ax[1, 0].set_ylabel("Model (dashed) and Total times")


def reset_event_loop():
    asyncio.set_event_loop(asyncio.new_event_loop())


class SearchTester:
    def __init__(
        self,
        start_nodes: Sequence[ExprWithEnv],
        sync_fn: Callable,
        coro_fn: Callable,
        model_config: Dict[str, Any],
        batch_size: int,
        benchmark_fn: Optional[Callable] = None,
    ):

        self._start_nodes = start_nodes
        self._coro_fn = coro_fn
        self._sync_fn = sync_fn
        self._model_config = model_config
        self._batch_size = batch_size
        self._tracker = MetricTracker()
        self._kwargs = dict(start_nodes=self._start_nodes, batch_size=self._batch_size)
        self._expected = None
        self._benchmark_fn = benchmark_fn

    def do_plots(self, **kwargs):
        self._tracker.do_plots(max_batch_size=self._batch_size, **kwargs)

    def run(self, fn, is_async=True, name=None, use_model_config=False, **kwargs):
        if name is None:
            kwargs_str = ", ".join((f"{k}={v}" for k, v in sorted(kwargs.items())))
            name = fn.__name__
            if name.endswith("_implementation"):
                name = name[: -len("_implementation")]
            name = f"{name}({kwargs_str})"
        for k, v in sorted(kwargs.items()):
            print(f" {k} = {v}")

        # model / model_config
        if use_model_config:
            kwargs["model_config"] = self._model_config
        else:
            model = regressor_from_config(self._model_config)
            self._tracker.wrap_model(model)
            kwargs["model"] = model

        # coro_fn / sync_fn
        if is_async:
            reset_event_loop()
            kwargs["coro_fn"] = self._coro_fn
        else:
            kwargs["sync_fn"] = self._sync_fn
        assert not any(k in kwargs for k in self._kwargs.keys())
        kwargs.update(self._kwargs)
        with self._tracker.tracked(name):
            actual = fn(**kwargs)
        if self._expected is not None:
            assert actual == self._expected
        return actual

    def run_multi_server(self, **kwargs):
        self.run(
            multi_server_ray_implementation,
            is_async=True,
            use_model_config=True,
            **kwargs,
        )

    def run_sync(self):
        with self._tracker.group("sync"):
            if self._benchmark_fn is not None:
                self.run(self._benchmark_fn, is_async=False, name="knossos")

            self._expected = self.run(sync_implementation, is_async=False)

    def run_async(self):
        with self._tracker.group("single"):
            self.run(single_process_implementation, is_async=True)
        with self._tracker.group("mp"):
            for num_procs in (1, 2, 4, 8):
                self.run(
                    mp_implementation, is_async=True, num_procs=num_procs,
                )
        with self._tracker.group("ray"):
            for managed_queues in (False, True):
                for num_procs in (1, 2, 4):
                    self.run(
                        single_server_ray_implementation,
                        is_async=True,
                        num_procs=num_procs,
                        managed_queues=managed_queues,
                    )


def get_model_config():
    return dict(
        sparse_gnn=True,
        output_hidden_dim=200,
        num_propagations=10,
        hidden_dim=200,
        simulation_depth=49,
        force_gpu=True,
        seed_all_reps=0,
        use_subtree_match_edges=True,
    )


def get_scenario_kwargs(
    rules="ml_rules_no_bind", expr_set="ksc/blas/blas_combined.kso", num_nodes=8
):
    start_nodes = [
        e.exprenv for e in get_expression_set(expr_set).named_exprenvs()[-num_nodes:]
    ]
    return dict(rules=get_rules(rules), start_nodes=start_nodes)


def split_round_robin(values: Sequence, num_procs: Optional[int]) -> Sequence[Sequence]:
    """
    Split values into a `num_procs` sequences.

    ```python
    split_round_robin(range(10), 3) == [[0, 3, 6, 9], [1, 4, 7], [2, 5, 8]]
    ```
    """
    if num_procs is None:
        return [[vals] for vals in values]
    out = [[] for _ in range(num_procs)]
    for i, vals in enumerate(values):
        out[i % num_procs].append(vals)
    return out


def merge_round_robin(split_values: Sequence[Sequence]):
    """
    Merge results corresponding to splits from `split_round_robin`.

    ```python
    merge_round_robin([[0, 3, 6, 9], [1, 4, 7], [2, 5, 8]]) == list(range(10))
    ```
    """
    out = []
    max_len = max(len(v) for v in split_values)
    for i in range(max_len):
        for vals in split_values:
            if i == len(vals):
                return out
            out.append(vals[i])
    return out


def sync_adv_fn(model, batch_size) -> Callable[[Sequence[ExprWithEnv]], np.ndarray]:
    """Synchronous advantage function."""

    def adv_fn(exprs):
        if not exprs:
            return np.empty((0, model.num_time_heads), dtype=np.float32)
        out = []
        for i in range(0, len(exprs), batch_size):
            out.append(model.evaluate_all_time_left(exprs[i : i + batch_size]))
        return np.concatenate(out, axis=0)

    return adv_fn


def sync_implementation(
    start_nodes, sync_fn: Callable, model: ModelWrapper, batch_size: int
):
    return [
        sync_fn(start_node, sync_adv_fn(model, batch_size))
        for start_node in start_nodes
    ]


def run_async_fn(start_nodes, async_fn, coro_fn, num_remaining=None):
    task = asyncio.gather(
        *(coro_fn(start_node, async_fn) for start_node in start_nodes)
    )
    result = async_fn.run_until_complete(task)
    if num_remaining is not None:
        num_remaining.value -= 1
    return result


run_async_remote_fn = ray.remote(num_cpus=1)(run_async_fn)


def _preprocess_fn(batch, pipeline):
    return [(pipeline.prepare_example(expr), expr.cost()) for expr in batch]


def client_preprocess_fn(model):
    return functools.partial(_preprocess_fn, pipeline=model._graph_pipeline)


def _get_neighbors(expr: ExprWithEnv, rules: RuleSet):
    neighs = set((rw.apply(expr) for rw in rules.get_all_rewrites(expr)))
    if expr in neighs:
        neighs.remove(expr)
    return tuple(neighs)


def get_neighbors_fn(rules):
    return functools.partial(_get_neighbors, rules=rules)


def any_remaining(value):
    return value.value > 0


@ray.remote
def run_remote_model_server(
    model_config: Dict[str, Any],
    conn: Connection,
    cond: Callable,
    batcher,
    preprocess_on_client: bool,
    prefetch_buffer: int,
    gpu_memory_fraction: float,
):
    model_config["gpu_memory_fraction"] = gpu_memory_fraction
    run_model_server(
        model=regressor_from_config(model_config),
        conn=conn,
        cond=cond,
        batcher=batcher,
        preprocess_on_client=preprocess_on_client,
        prefetch_buffer=prefetch_buffer,
    )


def multi_server_ray_implementation(
    start_nodes: Sequence,
    coro_fn: Callable,
    model_config: Dict[str, Any],
    batch_size: int,
    prefetch_buffer: int = 1,
    num_servers: int = 1,
    num_procs: Optional[int] = 1,  # defaults to number of start_nodes
    total_gpu_frac: float = 0.6,
    managed_queues: bool = False,
):
    manager = rm.Manager()
    split_nodes = split_round_robin(start_nodes, num_procs)
    num_remaining = manager.Value(len(split_nodes))
    client_fns, conn = get_connected_functions(
        manager.Queue if managed_queues else RayQueue, len(split_nodes), False
    )
    tasks = [
        run_async_remote_fn.remote(split, client_fn, coro_fn, num_remaining)
        for split, client_fn in zip(split_nodes, client_fns)
    ]
    cond = functools.partial(any_remaining, value=num_remaining)
    per_gpu_frac = total_gpu_frac / num_servers
    remote_fn = run_remote_model_server.options(num_cpus=1, num_gpus=per_gpu_frac)
    for _ in range(num_servers):
        remote_fn.remote(
            model_config=model_config,
            conn=conn,
            cond=cond,
            batcher=BatchSizeBatcher(batch_size),
            preprocess_on_client=False,
            prefetch_buffer=prefetch_buffer,
            gpu_memory_fraction=per_gpu_frac,
        )
    results = ray.get(tasks)
    return merge_round_robin(results)


def single_process_implementation(
    start_nodes: Sequence[ExprWithEnv],
    coro_fn: Callable,
    model: ModelWrapper,
    batch_size: int,
    prefetch_buffer: int = 1,
):

    (client_fn,), server_conn = get_connected_functions(QueueQueue, 1, True)
    task = asyncio.gather(
        *(coro_fn(start_node, client_fn) for start_node in start_nodes)
    )
    loop = task.get_loop()

    def cond():
        return not task.done()

    def callback():
        client_fn.flush()
        loop.stop()
        loop.run_forever()

    run_model_server(
        model=model,
        conn=server_conn,
        cond=cond,
        batcher=BatchSizeBatcher(batch_size),
        prefetch_buffer=prefetch_buffer,
        callback=callback,
    )
    return task.result()


def single_server_ray_implementation(
    start_nodes: Sequence[ExprWithEnv],
    coro_fn: Callable,
    model: ModelWrapper,
    batch_size: int,
    prefetch_buffer: int = 1,
    num_procs: Optional[int] = None,
    managed_queues: bool = True,
):
    manager = rm.Manager()
    preprocess_fn = client_preprocess_fn(model)
    split_nodes = split_round_robin(start_nodes, num_procs)
    num_remaining = manager.Value(len(split_nodes))
    client_fns, conn = get_connected_functions(
        manager.Queue if managed_queues else RayQueue,
        len(split_nodes),
        True,
        preprocess_fn=preprocess_fn,
    )
    tasks = [
        run_async_remote_fn.remote(split, client_fn, coro_fn, num_remaining)
        for split, client_fn in zip(split_nodes, client_fns)
    ]

    cond = functools.partial(any_remaining, value=num_remaining)
    run_model_server(
        model=model,
        conn=conn,
        cond=cond,
        batcher=BatchSizeBatcher(batch_size),
        preprocess_on_client=True,
        prefetch_buffer=prefetch_buffer,
    )
    results = ray.get(tasks)
    return merge_round_robin(results)


def mp_implementation(
    start_nodes,
    coro_fn: Callable,
    model: ModelWrapper,
    batch_size: int,
    prefetch_buffer: int = 1,
    num_procs: Optional[int] = None,
):
    preprocess_fn = client_preprocess_fn(model)
    split_nodes = split_round_robin(start_nodes, num_procs)
    client_fns, conn = get_connected_functions(
        mp.Manager().Queue, len(split_nodes), True, preprocess_fn=preprocess_fn,
    )

    args = [
        (split, client_fn, coro_fn) for split, client_fn in zip(split_nodes, client_fns)
    ]
    with ProcessPoolExecutor() as executor:
        futs = [executor.submit(run_async_fn, *ar) for ar in args]
        run_model_server(
            model=model,
            conn=conn,
            cond=lambda: not all(f.done() for f in futs),
            batcher=BatchSizeBatcher(batch_size),
            prefetch_buffer=prefetch_buffer,
            preprocess_on_client=True,
        )
    results = [fut.result() for fut in futs]
    return merge_round_robin(results)


def astar_tester(
    model_config: Dict[str, Any],
    rules: RuleSet,
    start_nodes: Sequence[ExprWithEnv],
    total_gnn_budget: int = 16000,
    buffer_size: int = 16,
    batches_per_buffer: int = 8,
):

    gnn_budget = total_gnn_budget // len(start_nodes)
    shared_kwargs = dict(
        neighbors_fn=get_neighbors_fn(rules),
        simulation_depth=model_config["simulation_depth"],
        gnn_budget=gnn_budget,
        buffer_size=buffer_size,
    )

    sync_fn = functools.partial(ss.batched_astar_search, **shared_kwargs)
    coro_fn = functools.partial(batched_astar_search, **shared_kwargs)

    def benchmark_fn(start_nodes, model, batch_size, sync_fn=None):
        del sync_fn
        searcher = AStarSearcher(
            rules=rules,
            simulation_depth=model.num_time_heads - 1,
            max_gnn=gnn_budget,
            batch_size=batch_size // batches_per_buffer,
            expander_factory=Expander,
        )
        for start_node in start_nodes:
            searcher._search(model, np.random.default_rng(0), start_node)

    return SearchTester(
        start_nodes,
        sync_fn,
        coro_fn,
        model_config,
        buffer_size * batches_per_buffer,
        benchmark_fn=benchmark_fn,
    )


def _probs_fn(nodes, adv):
    costs = [n.cost() for n in nodes]
    assert len(adv.shape) == 1
    logits = np.array(costs) - adv
    logits -= np.max(logits)
    denom = -np.min(logits)
    if np.abs(denom) > 1:
        logits /= denom  # initial predictions are wild
    probs = np.exp(logits)
    probs /= np.sum(probs)
    return probs


def multi_rollout_tester(
    model_config: Dict[str, Any],
    rules: RuleSet,
    start_nodes: Sequence[ExprWithEnv],
    num_rollouts: int = 4,
    depth=16,
    seed=0,
    batch_size=64,
):

    shared_kwargs = dict(
        num_rollouts=num_rollouts,
        depth=depth,
        neighbors_fn=get_neighbors_fn(rules),
        probs_fn=_probs_fn,
        seed=seed,
    )
    sync_fn = functools.partial(ss.multi_rollout, **shared_kwargs)
    coro_fn = functools.partial(multi_rollout, **shared_kwargs)

    return SearchTester(start_nodes, sync_fn, coro_fn, model_config, batch_size)


def run_continuous_astar_search(
    start_node: ExprWithEnv,
    adv_fn: ConnectedFunction,
    neighbors_fn: Callable,
    simulation_depth: int,
    gnn_budget: int,
):

    loop = asyncio.get_event_loop()
    task = loop.create_task(
        continuous_astar_search(
            start_node,
            neighbors_fn,
            lambda exprenv: exprenv.cost(),
            adv_fn,
            simulation_depth,
            gnn_budget,
        )
    )
    while not task.done():
        adv_fn.flush()
        loop.stop()
        loop.run_forever()
    return task.result()


def test_continuous_astar(gnn_budget: int = 16000, prefetch_buffer: int = 1):
    # note this is the maximum batch size
    # inference will run with fewer examples if it is ready
    batch_size = 1024
    model = regressor_from_config(get_model_config())
    scenario_kwargs = get_scenario_kwargs(num_nodes=1)
    rules = scenario_kwargs["rules"]
    (start_node,) = scenario_kwargs["start_nodes"]
    tracker = MetricTracker()
    tracker.wrap_model(model)

    with ProcessPoolExecutor(1) as executor:
        (client_fn,), server_conn = get_connected_functions(
            mp.Manager().Queue, 1, True, maxsize=batch_size // 4
        )
        fut = executor.submit(
            run_continuous_astar_search,
            start_node,
            client_fn,
            get_neighbors_fn(rules),
            model.num_time_heads - 1,
            gnn_budget,
        )

        def cond():
            return not fut.done()

        with tracker.tracked("continuous async"):
            run_model_server(
                model=model,
                conn=server_conn,
                cond=cond,
                batcher=BatchSizeBatcher(batch_size),
                prefetch_buffer=prefetch_buffer,
            )
    return fut.result()


def main(_):
    FLAGS = flags.FLAGS

    tester_kwargs = dict(
        model_config=get_model_config(),
        **get_scenario_kwargs(num_nodes=FLAGS.num_searches),
    )
    if FLAGS.search == "astar":
        tester = astar_tester(total_gnn_budget=FLAGS.gnn_budget, **tester_kwargs)
    elif FLAGS.search == "rollout":
        tester = multi_rollout_tester(num_rollouts=FLAGS.num_rollouts, **tester_kwargs)
    else:
        print(f"search must be one of (astar, rollout), got {FLAGS.search}")
        sys.exit()

    if FLAGS.continuous:
        test_continuous_astar(FLAGS.gnn_budget)
    elif FLAGS.multi_server:
        ray.init(num_gpus=FLAGS.num_gpus, num_cpus=FLAGS.num_cpus)
        tester.run_multi_server(
            num_servers=FLAGS.num_servers,
            num_procs=FLAGS.num_procs,
            total_gpu_frac=FLAGS.total_gpu_frac,
            managed_queues=FLAGS.managed_queues,
        )
    else:
        ray.init(num_gpus=1, num_cpus=FLAGS.num_cpus)
        if not FLAGS.skip_sync:
            tester.run_sync()
        tester.run_async()
        # sync() uses larger batch size compared to knossos, so comparison is unfair
        # we calculate sync purely to compare results of async implementations.
        tester.do_plots(skip=("sync()",))
        if FLAGS.save_path:
            plt.savefig(FLAGS.save_path)
        else:
            plt.show()


if __name__ == "__main__":
    app.run(main)

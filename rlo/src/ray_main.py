# fmt: off
import cProfile
import os
import ray

from rlo import analytics
from rlo.config_utils import config_for_repetition, kwargs_from_config
from rlo.factory import seed_from_config, simul_search_curriculum_from_config, get_train_and_eval_exprs
from rlo.flags import make_config_for_scenario, make_parser, check_save_config, ray_run_arguments
from rlo.ray_worker import RayWorkerPool

def main():
    from rlo.summarize_logs import summarize_logs
    run_parser = make_parser(ray_run_arguments)
    run_args, _ = run_parser.parse_known_args()

    if run_args.workers_per_gpu > 1 and (
            run_args.gpu_memory_fraction is None or run_args.gpu_memory_fraction * run_args.workers_per_gpu > 1.0):
        # In fact it seems there may need to be some margin of extra space on the GPU after allocating each worker
        # but we haven't identified how much, or good defaults for gpu_memory_fraction, yet.
        raise ValueError("Must have --gpu_memory_fraction <= 1/workers_per_gpu")

    config = make_config_for_scenario(run_args.scenario, ray_run_arguments)

    ray.init(config['address'], **kwargs_from_config(config,
        required_keys=("log_to_driver", "num_cpus", "num_gpus"),
        optional_keys=(),
        renames=(("redis_token", "redis_password"),)))

    train_set, eval_set = get_train_and_eval_exprs(config)
    check_save_config(config, train_set.named_exprenvs(), eval_set.named_exprenvs())
    pool = RayWorkerPool(config, remote_timeout=config["ray_timeout"], local_task_limit=run_args.profile_local or 0)

    with analytics.log_events_to_files(os.path.join(config["result_save_path"], "head" + os.path.sep)):
        analytics.event("expression_summary", num_train_expr = len(train_set.named_exprenvs()), num_test_expr = len(eval_set.named_exprenvs()))
        for rep_config in ([config] if config.get("repetition") is not None
                else [config_for_repetition(config, repetition) for repetition in range(config["num_repetitions"])]):
            with analytics.Scope(repetition=rep_config['repetition']):
                curriculum = simul_search_curriculum_from_config(rep_config, train_set, eval_set)
                pool.schedule_work_requests_from(
                    curriculum.request_initial(seed_from_config(rep_config)))
        if (run_args.profile_local is None) or (run_args.profile_local > 0):
            # None means --profile_local was specified without a time limit
            cProfile.runctx("pool.run()", {}, {"pool": pool}, os.path.join(config["result_save_path"], "head", "prof.pstats"))
        else:
            pool.run()
    print("Run finished, {} live weights".format(len(pool._weight_id_map)))

    if run_args.timeline:
        ray.timeline(filename=os.path.join(config['result_save_path'], "ray_timeline.json"))
        ray.object_transfer_timeline(filename=os.path.join(config['result_save_path'], "ray_object_transfers.json"))
    ray.shutdown() # Reduce memory use of Ray while this headnode machine does all the plotting
    events = summarize_logs(config, eval_set, ray=True)

    if config["test_kill_worker_after_tasks"] >= 0:
        # Test mode - check the logs were sensible; otherwise, fail the run (after producing plots).
        # Note that these asserts are not guaranteed or even expected to hold for all parameter values.
        # Rather they are intended to allow writing useful tests via sensible choices of parameters.

        # First, check that at least one worker was killed. This is only guaranteed if the total number
        # of tasks is at least (num_workers * (test_kill_worker_after_tasks-1))+1.
        assert any(e["event"] == "worker_died" for e in events)

        # Second, check that at least one worker joined after the start.
        # Note that this doesn't check that the joining worker was one that had been killed (e.g. from
        # the same IP address); instead, another node might have connected for the first time instead.
        # Only if num_workers == num_repetitions (the number of workers required before we start),
        # can we be sure that the new-joiner was a reconnection.
        # Conversely, failing does not necessarily imply that such a worker cannot reconnect, merely that
        # it didn't (before the run finished). Only if the total number of tasks is greater than
        # (num_workers * test_kill_worker_after_tasks) can we be sure that at least one worker would *have* to
        # reconnect for the run to get this far.
        assert any(e["event"] == "worker_joined" for e in events)


if __name__ == "__main__": main()

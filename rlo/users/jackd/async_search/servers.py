"""Various functions for providing inferences to server connections."""
from typing import Callable, Iterable, Sequence, Optional
from queue import Empty as QueueEmpty
import time

import numpy as np
import tensorflow as tf
from ray.experimental.queue import Empty as RayEmpty

from rlo.tf_model import ModelWrapper
from batchers import Batcher, Unbatcher, BatchManager, I, O
from connections import Connection


Empties = (RayEmpty, QueueEmpty)


def identity(x):
    """No-op."""
    return x


def _populate_batch_manager(conn: Connection, batch_manager: BatchManager):
    """
    Move elements from conn to batch_manager.

    Terminates when `batch_manager` has a full batch of `conn` is empty.
    """
    while not batch_manager.has_full_batch():
        try:
            request = conn.recv(block=False)
            batch_manager.add_inputs(request)
        except (QueueEmpty, RayEmpty):
            break


def _batches(
    conn: Connection,
    batch_manager: BatchManager,
    cond: Callable[[], bool],
    allow_empty: bool = False,
    empty_timeout: float = 0.001,
) -> Iterable:
    """
    Batch entries from `conn`.

    Returns an iterable of batches as popped from `batch_manager`. Iteration terminates
    when `conn` is empty and `cond` is False. For efficiency, `cond` is only checked
    when there are no elements to yield.
    """
    while True:
        _populate_batch_manager(conn, batch_manager)
        if batch_manager.empty():
            if not cond():
                break
            time.sleep(empty_timeout)
            if allow_empty:
                yield batch_manager.pop_batch()
        else:
            yield batch_manager.pop_batch()


def run_function_server(
    fn: Callable[[Sequence[I]], Sequence[O]],
    conn: Connection[O, I],
    cond: Callable[[], bool],
    batcher: Batcher[I],
    unbatcher: Optional[Unbatcher[O]] = None,
    allow_empty: bool = True,
    empty_timeout: float = 0.001,
):
    """
    Run a basic function server.

    Args:
        fn: batched functions.
        conn: connection that serves fn requests.
        cond: callable condition for which to continue even when `conn` is empty.
        batcher: see `batchers.Batcher`.
        unbatcher: see `batchers.Unbatcher`. If `None`, assumes output is a numpy array.
        allow_empty: if True, `fn` may be called on empty batches when `conn` is empty
            but `cond()` is still True.
        empty_timeout: number of seconds to sleep for when both `batcher` and `conn` are
            empty but `cond` still returns True.
    """
    batch_manager = BatchManager(batcher, unbatcher)
    for batch in _batches(
        conn,
        batch_manager,
        cond=cond,
        allow_empty=allow_empty,
        empty_timeout=empty_timeout,
    ):
        for response in batch_manager.process_output(fn(batch)):
            conn.send(response)


def run_tf_server(
    preprocess_fn: Callable,
    tf_fn: Callable,
    batched_spec,
    conn: Connection,
    cond: Callable[[], bool],
    batcher: Batcher,
    unbatcher: Optional[Unbatcher] = None,
    prefetch_buffer: int = 1,
    callback: Callable = lambda: None,
    empty_timeout: float = 0.001,
):
    """
    Run a tensorflow function server with prefetched datasets.

    Args:
        preprocess_fn: function applied to each batch.
        tf_fn: tensorflow function operating on each batch of tensors.
        batched_spec: structure of `tf.TensorSpec`s describing the output of
            `preprocess_fn` / input of `tf_fn`.
        conn: `Connection` to serve.
        cond: callable condition for which to continue even when `conn` is empty.
        batcher: see `batchers.Batcher`.
        unbatcher: see `batchers.Unbatcher`. If `None`, assumes output is a numpy array.
        prefetch_buffer: see `tf.data.Dataset.prefetch`
        callback: called after each batch has been processed.
        empty_timeout: number of seconds to sleep for when both `batcher` and `conn` are
            empty but `cond` still returns True.
    """
    batch_manager = BatchManager(batcher, unbatcher)

    def generator():
        for batch in _batches(
            conn,
            batch_manager,
            cond=cond,
            allow_empty=True,
            empty_timeout=empty_timeout,
        ):
            yield preprocess_fn(batch)

    dataset = tf.data.Dataset.from_generator(
        generator,
        tf.nest.map_structure(lambda spec: spec.dtype, batched_spec),
        tf.nest.map_structure(lambda spec: spec.shape, batched_spec),
    )
    dataset = dataset.prefetch(prefetch_buffer)
    for batch in dataset:
        outputs = tf_fn(batch)
        if len(outputs) > 0:
            for response in batch_manager.process_output(outputs):
                conn.send(response)
        callback()


def run_model_server(
    model: ModelWrapper,
    conn: Connection,
    cond: Callable[[], bool],
    batcher: Batcher,
    unbatcher: Optional[Unbatcher] = None,
    prefetch_buffer: int = 1,
    preprocess_on_client: bool = False,
    callback: Callable = lambda: None,
    empty_timeout: float = 0.001,
):
    """
    Wrapper around `run_tf_server` when using a knossos `ModelWrapper`.

    Args: same as run_tf_server, except
        preprocess_on_client: if True, elements received on `conn` are assumed to be
            the output of `model._graph_pipeline.prepare_example`.
    """
    pipeline = model._graph_pipeline
    cost_norm = model._training_pipeline._cost_norm
    cost_spec = tf.TensorSpec((None,), tf.float32)
    spec = pipeline.batched_spec, cost_spec

    def empty_array(spec):
        return np.empty(
            [0 if s is None else s for s in spec.shape], dtype=spec.dtype.as_numpy_dtype
        )

    empty_el = tf.nest.map_structure(empty_array, spec)

    def preprocess_fn(batch: Sequence):
        if len(batch) == 0:
            return empty_el
        if preprocess_on_client:
            batch, costs = zip(*batch)
        else:
            costs = [expr.cost() for expr in batch]
            batch = [pipeline.prepare_example(expr) for expr in batch]
        graph_data = pipeline.post_batch_map(pipeline.zip_elements(batch))
        costs = tf.convert_to_tensor(costs, tf.float32)
        return graph_data, costs

    def model_fn(data):
        model_inputs, costs = data
        if len(costs) == 0:
            return np.empty(shape=(0, model.num_time_heads), dtype=np.float32)
        adv = model._evaluate_all_time_left(model_inputs)
        values = np.array(cost_norm.denormalize_values(adv.numpy(), costs))
        return values

    return run_tf_server(
        preprocess_fn=preprocess_fn,
        tf_fn=model_fn,
        batched_spec=spec,
        conn=conn,
        cond=cond,
        batcher=batcher,
        unbatcher=unbatcher,
        prefetch_buffer=prefetch_buffer,
        callback=callback,
        empty_timeout=empty_timeout,
    )

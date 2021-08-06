import time
from typing import Callable, Optional, Union

from rlo import factory
from rlo.tf_model import Weights
from rlo.model.model import ModelState as TorchModelState
from rlo.worker import Accumulator, WorkerPool

ModelState = Union[TorchModelState, Weights]


def measure_time(func: Callable, time_acc: Optional[Accumulator], *args, **kwargs):
    """ Runs func(*args,**kwargs), returning the time taken and also adding it to `time_acc` if the latter is non-None. """
    start = time.time()
    func(*args, **kwargs)
    taken = time.time() - start
    if time_acc is not None:
        time_acc.add(taken)
    return taken


class WorkerWithModel:
    """ An object that contains a ModelWrapper, and manages loading a new set of weights. """

    def __init__(self, config):
        self._config = config
        self._model_wrapper = None

    def _set_weights_or_seed(self, weights_or_seed: Union[ModelState, int, None]):
        def create_model(seed: int = 0xFFFF):
            # We always have to supply some seed to create a model,
            # even if we don't care and will immediately overwrite the weights.
            self._model_wrapper = factory.regressor_from_config(
                {**self._config, "seed_all_reps": seed}
            )

        if isinstance(weights_or_seed, int):
            if self._model_wrapper is not None:
                self._model_wrapper.__exit__(None, None, None)
                self._model_wrapper = None
            create_model(weights_or_seed)
        else:
            if self._model_wrapper is None:
                create_model()
            if weights_or_seed is not None:
                self._model_wrapper.set_weights(weights_or_seed)  # type: ignore

    def __enter__(self):
        return self

    def __exit__(self, ex_type, ex, ex_trace):
        if self._model_wrapper is None:
            return False  # Do not suppress any exception
        return self._model_wrapper.__exit__(ex_type, ex, ex_trace)

    def exec(self, weights_or_seed: Union[ModelState, int, None], func):
        start_time = time.time()
        self._set_weights_or_seed(weights_or_seed)
        weights_loaded_time = time.time()
        res = func(self._model_wrapper)
        finish_time = time.time()
        return res, weights_loaded_time - start_time, finish_time - weights_loaded_time


class LocalWorker(WorkerPool, WorkerWithModel):
    def __init__(self, config):
        WorkerPool.__init__(self)
        WorkerWithModel.__init__(self, config)

    def run(self):
        while not self._items.empty():
            item = self._items.get()
            # Run in the logging scope captured at the time the WorkItem was submitted
            with item.logging_scope:
                res, _weight_load_time, exec_time = self.exec(item.weights, item.func)
            if item.time_acc is not None:
                item.time_acc.add(exec_time)
            measure_time(item.run_continuation, item.time_acc, res)

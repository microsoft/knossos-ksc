from __future__ import annotations

import os
from typing import Dict, Iterable, Optional
import abc
from dataclasses import dataclass
import copy

import torch
import numpy as np

from rlo.expression_util import ExprWithEnv
from rlo import worker
from rlo.torch_graph_data import (
    DataConverter,
    BatchedGraphDataWithTarget,
    BatchedGraphData,
)
from .losses import LossFunc


class OptimizerAndLossNotInitialized(RuntimeError):
    pass


class TorchModelWrapper:
    """Wrap a torch model for use in search."""

    def __init__(
        self, model: Model, data_converter: DataConverter, device: torch.device,
    ):
        self._data_converter = data_converter
        self._model = model
        self.to(device)

    def to(self, device: torch.device):
        # Put model and data on the specified device
        self.model.to(device)
        self.data_converter.to(device)

    @property
    def model(self):
        return self._model

    @property
    def data_converter(self):
        return self._data_converter

    def get_weights(self):
        return self.model.get_weights()

    def set_weights(self, model_state: ModelState) -> None:
        # TODO rename to make it obvious this loads optimizer state, too.
        # For now, use the same method name as tensorflow code.
        self.model.set_weights(model_state)

    def __enter__(self):
        return self

    def __exit__(self, ex_type, exc, ex_trace):
        return False  # Do not suppress any exception

    def evaluate_all_time_left(self, expr_batch: Iterable[ExprWithEnv]) -> np.ndarray:
        self._model.eval()
        graphs = [self._data_converter.expr_to_graph(e) for e in expr_batch]
        batch = BatchedGraphData.collate(graphs)
        model_output = self._model(batch)
        return self._data_converter.denormalize_and_numpify(model_output, expr_batch)

    def as_train_search_model(self):
        return self

    def as_eval_search_model(self):
        return self

    def load_weights(self, path: str) -> None:
        self.model.load_weights(path)


class Model(torch.nn.Module, abc.ABC):
    """Torch.nn.Module with some methods for training and evaluation, like a pytorch LightningModule.
    Device is specified one level up, in TorchModelWrapper, not here."""

    def __init__(self) -> None:
        super().__init__()
        # Child classes should initialize self._optimizer in __init__
        # TODO make optimizer not belong to Model. It's like this for now because the way we restore optimizer
        # state from one generation to the next is via a Model instance; see `request_distill` method of ModelState
        # and code in worker.py.
        self._optimizer: Optional[torch.optim.Optimizer] = None
        self._loss_func: Optional[LossFunc] = None

    @abc.abstractmethod
    def init_optimizer_and_loss(self):
        # Implement this method to populate self._optimizer and self._loss_func
        pass

    def has_optimizer_and_loss(self) -> bool:
        return self._optimizer is not None and self._loss_func is not None

    @property
    def optimizer(self):
        return self._optimizer

    def evaluate_training_loss(self, batch: BatchedGraphDataWithTarget) -> torch.Tensor:
        """Compute loss for a single batch.  As a side-effect, puts the model in `train` mode.
        """
        return self._evaluate_loss(batch, train=True)

    def evaluate_validation_loss(
        self, batch: BatchedGraphDataWithTarget
    ) -> torch.Tensor:
        """Compute loss for a single batch. As a side-effect, puts the model in `eval` mode.
        
        It's the caller's responsibility to wrap this in `torch.no_grad`.
        """
        return self._evaluate_loss(batch, train=False)

    def _evaluate_loss(self, batch: BatchedGraphDataWithTarget, train: bool):
        """Compute loss for a single batch."""
        if self._loss_func is None:
            raise OptimizerAndLossNotInitialized(
                "Call init_optimizer_and_loss before trying to evaluate loss."
            )
        self.train(train)
        predictions = self.forward(batch.graph)
        return self._loss_func(  # pylint: disable=not-callable
            predictions, batch.target, batch.weight
        )

    @abc.abstractmethod
    def forward(self, batch: BatchedGraphData) -> torch.Tensor:
        raise NotImplementedError

    def set_weights(self, weights: ModelState) -> None:
        # TODO rename to make it obvious this restores optimizer state, too. For now, use same method name as tensorflow code.
        if weights.optimizer_state_dict is not None:
            if self._optimizer is None:
                self.init_optimizer_and_loss()
            self.optimizer.load_state_dict(weights.optimizer_state_dict)
        self.load_state_dict(weights.model_state_dict)  # type: ignore[arg-type]

    def get_weights(self):
        """Get a copy of the model and optimizer state. Optimizer state will be None if optimizer has not been initialized."""
        return copy.deepcopy(ModelState.from_model(self))

    def load_weights(self, path: str):
        model_state = ModelState.from_path(path)
        self.set_weights(model_state)


@dataclass(frozen=True)
class ModelState:
    """Weights and optimizer state"""

    model_state_dict: Dict[str, torch.Tensor]
    optimizer_state_dict: Optional[Dict]

    @classmethod
    def from_model(cls, model: Model):
        # Get the model and optimizer state. Returns references, not copies.
        return cls(
            model_state_dict=model.state_dict(),
            optimizer_state_dict=None
            if model.optimizer is None
            else model.optimizer.state_dict(),
        )

    def save(self, path: str):
        print("Saving model to {}".format(path))
        os.makedirs(os.path.dirname(path), exist_ok=True)
        torch.save(
            {
                "model_state_dict": self.model_state_dict,
                "optimizer_state_dict": self.optimizer_state_dict,
            },
            path,
        )

    @classmethod
    def from_path(cls, path):
        if torch.cuda.is_available():
            state = torch.load(path)
        else:
            state = torch.load(path, map_location=torch.device("cpu"))
        return cls(**state)

    def request_distill(
        self, name_suffix, distillator, data, seed, urgency, time_acc, scope
    ):
        """Making request_distill a method of weights is a trick to get Ray to work nicely with DualModelInterpreter"""
        new_weights = yield worker.RunOnGPU(
            "distill_" + name_suffix,
            self,
            lambda net: distillator(model_wrapper=net, seed=seed, dataset=data),
            urgency=urgency,
            time_acc=time_acc,
            scope=scope,
        )
        return new_weights

"""
Reimplements distillator.py with a callback-based interface.
Author: Dominic Jack
Date: May 2020
"""
import itertools
import time
from typing import Dict, Iterable, List, Optional, Sequence, Union

import numpy as np
import tensorflow as tf
import tqdm

from rlo import analytics
from rlo import distillator
from rlo import utils
from rlo.dataset import PolicyNetDataset, StateValueDataset
from rlo.tf_model import ModelWrapper, Weights


class StopTraining(Exception):
    """
    Exception to signal training should stop.

    Like `StopIteration` it should not be interpreted as an error.
    """


# pylint: disable=unused-argument
class TqdmCallback(tf.keras.callbacks.Callback):
    def on_epoch_begin(self, epoch, logs=None):
        self._time = time.time()
        self._progress = tqdm.tqdm(desc="Epoch {}".format(epoch))

    def on_batch_end(self, batch, logs=None):
        self._progress.update()

    def on_epoch_end(self, epoch, logs=None):
        self._progress.close()
        print("-------")
        if logs is not None:
            for key in sorted(logs):
                print("  {:15s}: {}".format(key, logs[key]))
        print("  {:15s}: {}".format("total time (s)", time.time() - self._time))


class DistillationCallback(tf.keras.callbacks.Callback):
    def __init__(self, model: ModelWrapper, min_epochs: int, patience_epochs: int):
        self.min_epochs = min_epochs
        self.patience_epochs = patience_epochs
        self.rlo_model = model

    def on_train_begin(self, logs: Optional[Dict] = None):
        self.epoch_counter = 0
        self.batch_counter = 0
        self.best_valid_loss: Optional[float] = np.inf
        self.train_loss_at_best: Optional[float] = np.inf
        self.last_train_loss = None
        self.last_valid_loss = None
        self.best_valid_epoch = self.epoch_counter

    def on_epoch_begin(self, epoch: int, logs=None):
        self.batch_losses = []  # type: List[float]

    def on_batch_end(self, batch: int, logs: Optional[Dict] = None):
        if logs is not None:
            self.batch_losses.append(logs["loss"])

    def on_epoch_end(self, epoch: int, logs: Optional[Dict] = None):
        if logs is not None:
            self.last_train_loss = logs["weighted_loss"]
            self.last_valid_loss = logs["val_weighted_loss"]
            self.last_train_total_weight = logs["total_weight"]
            self.last_valid_total_weight = logs["val_total_weight"]
        else:
            self.last_train_loss = None
            self.last_valid_loss = None
            self.last_train_total_weight = None
            self.last_valid_total_weight = None
        analytics.event(
            "distill_epoch",
            verbosity=1,
            epoch_counter=epoch,
            train_loss=self.last_train_loss,
            train_losses=self.batch_losses,
            valid_loss=self.last_valid_loss,
            prev_best_valid_loss=self.best_valid_loss,
            prev_best_valid_epoch=self.best_valid_epoch,
        )

        self.batch_counter += len(self.batch_losses)

        if self.last_valid_loss is None or self.last_valid_loss < self.best_valid_loss:
            self.best_valid_loss = self.last_valid_loss
            self.best_valid_epoch = epoch
            self.train_loss_at_best = self.last_train_loss
            self.best_weights = self.rlo_model.get_weights()

        self.batch_losses = []
        self.epoch_counter = epoch  # used in distill_end event
        if epoch >= max(self.min_epochs, self.best_valid_epoch + self.patience_epochs):
            self.model.stop_training = True

    def on_train_end(self, logs: Optional[Dict] = None):
        # Hardcoding "inf" here means to always keep the model trained
        # for up to patience iters BEYOND the point of lowest validation loss; this seems wrong, but the correct
        # value of 0/negative (i.e. always restore the "best" params) more than doubled execution time (issue #224).
        # This was resolved in distillator.py in PR #1236 (effectively changing the inf to 0).
        # Inbetween values allow only to restore weights where it has most impact, if we want to introduce that additional hyperparameter!
        assert self.last_valid_loss is not None
        if self.last_valid_loss / self.best_valid_loss > np.inf:
            self.rlo_model.set_weights(self.best_weights)
        else:
            self.best_weights = self.rlo_model.get_weights()
        analytics.event(
            "distill_end",
            epoch_counter=self.epoch_counter + 1,  # finished epochs
            batch_counter=self.batch_counter,
            train_total_weight=self.last_train_total_weight,
            loss=self.last_train_loss,
            valid_loss=self.last_valid_loss,
            valid_total_weight=self.last_valid_total_weight,
            best_valid_epoch=self.best_valid_epoch,
            valid_loss_at_best=self.best_valid_loss,
            loss_at_best=self.train_loss_at_best,
        )


def _fit(
    model_wrapper: ModelWrapper,
    train_data: Iterable,
    validation_data: Iterable,
    callbacks: Sequence[tf.keras.callbacks.Callback],
    max_epochs: Optional[int],
) -> None:
    """
    Minimal rlo equivalent of tf.keras.Model.fit.

    Runs until StopTraining is raised by a callback.

    See Distillator.__call__ for example usage.

    Args:
        model_wrapper: model to be trained.
        train_data: iterable of batched data which can be iterated over multiple times.
        validation_data: iterable of batch data for validation that can be iterated over
            multiple times.
        callbacks: sequence of `tf.keras.callbacks.Callback` for custom functionality.
    """
    training_model = model_wrapper.keras_model
    training_model.stop_training = False
    for callback in callbacks:
        callback.set_model(training_model)
    try:
        logs = {}  # type: Dict[str, float]
        for cb in callbacks:
            cb.on_train_begin(logs)
        for epoch in itertools.count() if max_epochs is None else range(max_epochs):
            model_wrapper.reset_metrics()
            for cb in callbacks:
                logs = {}
                cb.on_epoch_begin(epoch, logs)

            # training
            for index, train_batch in enumerate(train_data):
                if training_model.stop_training:
                    raise StopTraining()
                logs = {}
                for cb in callbacks:
                    cb.on_train_batch_begin(index, logs)
                logs["loss"] = model_wrapper.train_on_batch(train_batch)
                for cb in callbacks:
                    cb.on_train_batch_end(index, logs)
            logs = model_wrapper.metric_results()
            # validation
            model_wrapper.reset_metrics()
            for val_batch in validation_data:
                model_wrapper.evaluate_loss(val_batch)
            logs.update(
                {f"val_{k}": v for k, v in model_wrapper.metric_results().items()}
            )
            for cb in callbacks:
                cb.on_epoch_end(epoch, logs)
    except StopTraining:
        pass
    logs = {}
    for cb in callbacks:
        cb.on_train_end(logs)


class DistillatorV2(distillator.Distillator):
    def __init__(
        self,
        min_epochs: int,
        max_epochs: Optional[int] = None,
        batch_size: Optional[int] = None,
        max_nodes_per_batch: Optional[int] = None,
        split: float = 0.9,
        patience_epochs: int = 10,
        verbose: bool = False,
        tb_dir: Optional[str] = None,
    ):
        super().__init__(
            max_epochs=max_epochs,
            min_epochs=min_epochs,
            batch_size=batch_size,
            max_nodes_per_batch=max_nodes_per_batch,
            split=split,
            patience_epochs=patience_epochs,
        )
        self._verbose = verbose
        self._tb_dir = tb_dir

    def __call__(
        self,
        model_wrapper: ModelWrapper,
        seed: int,
        dataset: Union[PolicyNetDataset, StateValueDataset],
    ) -> Weights:
        """
        This function takes the model and distill the information content of dataset into the current model.
        """
        analytics.event(
            "distill_start",
            num_points=dataset.num_points(),
            num_exprs=dataset.num_expressions(),
            seed=seed,
        )
        dataset_list = dataset.get_examples()

        rng = utils.rng(seed)
        dataset_list = utils.permutation(rng, dataset_list)
        train_examples, val_examples = distillator.split_train_valid(
            self.split, dataset_list
        )
        del dataset_list  # ensure gc can clean up

        train_data = model_wrapper.create_dataset(
            train_examples, rng, **self._regen_kwargs
        )
        val_data = model_wrapper.create_dataset(val_examples, **self._regen_kwargs)
        callback = DistillationCallback(
            model_wrapper, self.min_epochs, self.patience_epochs
        )
        callbacks = [callback]
        if self._verbose:
            callbacks.append(TqdmCallback())
        if self._tb_dir is not None:
            callbacks.append(
                tf.keras.callbacks.TensorBoard(
                    log_dir=self._tb_dir, profile_batch="2, 12"
                )
            )
        _fit(model_wrapper, train_data, val_data, callbacks, max_epochs=self.max_epochs)
        distillator.log_fitted_vals(model_wrapper, dataset, **self._regen_kwargs)
        return callback.best_weights

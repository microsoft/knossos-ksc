"""
This file implements a training routine to fit the model to a dataset.

The main function train_model() takes a model, a dataset, and a specification for
training, and performs a number of epochs of training on the model.
"""
from dataclasses import dataclass
import itertools
from typing import List, NamedTuple, Optional, Tuple

import torch

from rlo import analytics, utils
from rlo.dataset import (
    RawValueExample,
    StateValueDataset,
)
from rlo.distillator import fitted_values_to_list, split_train_valid
from rlo.model.model import ModelState, TorchModelWrapper
from rlo.torch_graph_data import create_dataloader


class TrainingConfig(NamedTuple):
    min_epochs: int
    batch_size: Optional[int] = None  # Number of graphs per batch
    max_nodes_per_batch: Optional[int] = None
    max_epochs: Optional[int] = None
    patience_epochs: int = 10
    split: float = 0.9  # proportion of data used for training


def train_model(
    config: TrainingConfig,
    seed: int,
    model_wrapper: TorchModelWrapper,
    dataset: StateValueDataset,
) -> ModelState:
    """Fit a model to a dataset.
    
    The `seed` parameter is used for data splitting, shuffling, and stochastic elements of model forward pass e.g. dropout.
    
    Returns:
        ModelState: model state and optimizer state.
    """
    model = model_wrapper.model
    data_converter = model_wrapper.data_converter

    raw_examples = dataset.get_examples()

    # Shuffle the data and split into train & validation.
    examples_shuffled = utils.permutation(utils.rng(seed), raw_examples)

    raw_train_examples, raw_valid_examples = split_train_valid(
        examples_shuffled=examples_shuffled, split=config.split
    )
    train_examples = [
        data_converter.prepare_training_example(e) for e in raw_train_examples
    ]
    valid_examples = [
        data_converter.prepare_training_example(e) for e in raw_valid_examples
    ]

    # Set up data loaders
    train_dataloader = create_dataloader(
        train_examples,
        batch_size=config.batch_size,
        max_nodes=config.max_nodes_per_batch,
        rng=utils.torch_rng(seed + 1),
    )
    valid_dataloader = create_dataloader(
        valid_examples,
        batch_size=config.batch_size,
        max_nodes=config.max_nodes_per_batch,
        rng=None,
    )

    if not model.has_optimizer_and_loss():
        # If the model already has optimizer and loss, it's restored from previous generation and we don't want to overwrite it.
        model.init_optimizer_and_loss()
    optimizer = model.optimizer

    # Initialize validation loss etc. for early stopping
    best_valid_loss = float("inf")
    best_valid_epoch = 0
    # Keep track of the model state with the best validation loss so far
    best_state = ModelState.from_model(model)
    valid_loss = float("nan")
    train_loss_at_best = float("nan")
    batch_losses: List[float] = []
    batch_counter = 0

    with utils.random_state_context(seed):
        # Use `random_state_context` because dropout may advance the base random generator state.

        for epoch_counter in (
            itertools.count() if config.max_epochs is None else range(config.max_epochs)
        ):
            if (
                epoch_counter >= config.min_epochs
                and epoch_counter >= best_valid_epoch + config.patience_epochs
            ):  # Stop training if validation loss hasn't improved in `patience_epochs` iterations
                break

            # Train over batches
            # No need to put model in training/eval mode here: evaluate_training_loss and
            # evaluate_validation_loss do that.

            accumulated_loss = MetricAccumulator()
            batch_losses = []
            for batch in train_dataloader:
                optimizer.zero_grad()
                loss = model.evaluate_training_loss(batch)
                loss.backward()
                optimizer.step()
                loss_float = loss.item()
                batch_losses.append(loss_float)
                batch_weight = torch.sum(batch.weight).item()
                accumulated_loss += (loss_float, batch_weight)

            batch_counter += len(batch_losses)

            train_loss = accumulated_loss.weighted_mean()
            train_total_weight = accumulated_loss.weight

            # Measure loss on validation data
            accumulated_loss = MetricAccumulator()
            with torch.no_grad():
                for batch in valid_dataloader:
                    loss = model.evaluate_validation_loss(batch)
                    loss_float = loss.item()
                    batch_weight = torch.sum(batch.weight).item()
                    accumulated_loss += (loss_float, batch_weight)

            valid_loss = accumulated_loss.weighted_mean()
            valid_total_weight = accumulated_loss.weight

            analytics.event(
                "distill_epoch",
                verbosity=1,
                epoch_counter=epoch_counter,
                train_loss=train_loss,
                train_total_weight=train_total_weight,
                train_losses=batch_losses,
                valid_loss=valid_loss,
                valid_total_weight=valid_total_weight,
                prev_best_valid_loss=best_valid_loss,
                prev_best_valid_epoch=best_valid_epoch,
            )

            if valid_loss < best_valid_loss:
                best_valid_loss = valid_loss
                best_valid_epoch = epoch_counter
                train_loss_at_best = train_loss
                best_state = model.get_weights()
            epoch_counter += 1

    # Restore weights from the best epoch (but not optimizer state)
    model.load_state_dict(best_state.model_state_dict)

    analytics.event(
        "distill_end",
        loss=train_loss,
        epoch_counter=epoch_counter + 1,  # completed epochs
        batch_counter=batch_counter,
        valid_loss=valid_loss,
        train_total_weight=train_total_weight,
        valid_total_weight=valid_total_weight,
        best_valid_epoch=best_valid_epoch,
        valid_loss_at_best=best_valid_loss,
        loss_at_best=train_loss_at_best,
    )

    log_fitted_vals(
        model_wrapper=model_wrapper,
        raw_examples=raw_train_examples + raw_valid_examples,
    )

    return best_state


def log_fitted_vals(
    model_wrapper: TorchModelWrapper, raw_examples: List[RawValueExample],
):
    with torch.no_grad():
        predictions = [
            model_wrapper.evaluate_all_time_left([e.exprenv])[0] for e in raw_examples
        ]

    analytics.event(
        "distill_fit",
        target_and_fitted={
            str(e.exprenv.expr): list(
                zip(e.values.tolist(), fitted_values_to_list(fitted))
            )
            for e, fitted in zip(raw_examples, predictions)
        },
        verbosity=1,
    )


@dataclass
class MetricAccumulator:
    """Accumulate metric and weight."""

    value: float = 0.0
    weight: float = 0.0

    def __iadd__(self, val_weight: Tuple[float, float]):
        self.value += val_weight[0]
        self.weight += val_weight[1]
        return self

    def weighted_mean(self):
        return self.value / self.weight

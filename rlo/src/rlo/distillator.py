# mypy: ignore-errors
"""
This file implements the distillator to distill the information of the provided dataset in the
weights of a neural network.
"""
import itertools
import math
from typing import Optional, Union

from rlo import analytics
from rlo.dataset import PolicyNetDataset, StateValueDataset
from rlo.tf_model import ModelWrapper
from rlo.policy_value_model import RawPolicyValueEvaluation
from rlo import utils


def fitted_values_to_list(fitted):
    return (
        fitted.values.tolist()
        if isinstance(fitted, RawPolicyValueEvaluation)
        else fitted.tolist()
    )


def log_fitted_vals(
    model_wrapper: ModelWrapper,
    dataset: Union[StateValueDataset, PolicyNetDataset],
    **regen_kwargs
):
    examples = dataset.get_examples()
    vals = model_wrapper.evaluate_in_batches(
        [e.exprenv for e in examples], **regen_kwargs
    )
    analytics.event(
        "distill_fit",
        target_and_fitted={
            str(e.exprenv.expr): list(
                zip(e.values.tolist(), fitted_values_to_list(fitted))
            )
            for e, fitted in zip(examples, vals)
        },
        verbosity=1,
    )


# fmt: off

class Distillator:
    def __init__(
            self,
            min_epochs: int,
            max_epochs: Optional[int] = None,
            batch_size: Optional[int] = None,
            max_nodes_per_batch: Optional[int] = None,
            split: float=0.9,
            patience_epochs: int=10,
            ):
        if (batch_size is None) == (max_nodes_per_batch is None):
            raise ValueError(
                "Exactly one of max_nodes_per_batch and batch_size must be given, "
                "got {} and {}.".format(max_nodes_per_batch, batch_size))
        self.min_epochs = min_epochs
        self.max_epochs = max_epochs
        self.split = split
        self.patience_epochs = patience_epochs
        self._regen_kwargs = dict(max_nodes=max_nodes_per_batch, batch_size=batch_size)



    def __call__(self, model_wrapper: ModelWrapper, seed: int, dataset: Union[StateValueDataset, PolicyNetDataset]):
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
        rng.shuffle(dataset_list)
        train_examples, valid_examples = split_train_valid(self.split, dataset_list)
        del dataset_list  # ensure gc can clean up
        train_examples = model_wrapper.create_dataset(
            train_examples, rng, **self._regen_kwargs)
        valid_examples = model_wrapper.create_dataset(valid_examples, **self._regen_kwargs)

        best_valid_loss = float("inf")
        best_valid_epoch = 0
        best_weights = model_wrapper.get_weights()
        valid_loss = float("nan")
        train_loss_at_best = float("nan")
        batch_losses = []
        batch_counter = 0
        for epoch_counter in itertools.count() if self.max_epochs is None else range(self.max_epochs):
            if epoch_counter >= max(self.min_epochs, best_valid_epoch + self.patience_epochs):
                break
            # Train over batches
            model_wrapper.reset_metrics()
            batch_losses=[model_wrapper.train_on_batch(batch) for batch in train_examples]
            train_metrics = model_wrapper.metric_results()
            train_loss = train_metrics['weighted_loss']
            train_total_weight = train_metrics['total_weight']

            batch_counter += len(batch_losses)
            # Compute validation loss
            model_wrapper.reset_metrics()
            for batch in valid_examples:
                model_wrapper.evaluate_loss(batch)
            valid_metrics = model_wrapper.metric_results()
            valid_loss = valid_metrics['weighted_loss']
            valid_total_weight = valid_metrics['total_weight']
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
                prev_best_valid_epoch=best_valid_epoch)

            if valid_loss < best_valid_loss:
                best_valid_loss = valid_loss
                best_valid_epoch = epoch_counter
                train_loss_at_best = train_loss
                best_weights = model_wrapper.get_weights()
            epoch_counter += 1

        model_wrapper.set_weights(best_weights)

        analytics.event("distill_end",
            loss=train_loss,
            epoch_counter=epoch_counter + 1,  # completed epochs
            batch_counter=batch_counter,
            valid_loss=valid_loss,
            train_total_weight=train_total_weight,
            valid_total_weight=valid_total_weight,
            best_valid_epoch=best_valid_epoch,
            valid_loss_at_best=best_valid_loss,
            loss_at_best=train_loss_at_best)

        log_fitted_vals(model_wrapper, dataset, **self._regen_kwargs)

        return best_weights


def split_train_valid(split, examples_shuffled):
    """
    Prepare train/validation data for training. Data should be shuffled before splitting: this function will not shuffle.
    
    Returns:
        train_data: iterable of batched data, reshuffled each iteration.
        validation_data: iterable of batched data for validation.
    """
    N = len(examples_shuffled)
    if N == 0: raise ValueError("No examples!!")
    num_train = math.ceil(N * split)  # If only one example, make sure it's training.

    # logging out train - validation information
    examples_shuffled_dataset = [(str(e.exprenv.expr), list(e.values)) for e in examples_shuffled]
    analytics.event("distill_train_split", dataset=examples_shuffled_dataset[:num_train], verbosity=1)
    analytics.event("distill_valid_split", dataset=examples_shuffled_dataset[num_train:], verbosity=1)
    return examples_shuffled[:num_train], examples_shuffled[num_train:]

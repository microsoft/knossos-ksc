# mypy: ignore-errors
import abc

from itertools import islice
from typing import Dict, Iterable, Optional, List, Tuple, Union

import numpy as np
import tensorflow as tf

from rlo import analytics
from rlo import losses
from rlo import metrics as metrics_lib
from rlo import utils
from rlo.expression import Expression
from rlo.expression_util import ExprWithEnv
from rlo.pipelines import RawExample, TrainingPipeline
from rlo import worker


def _init_optimizer_vars(optimizer, model_weights):
    """
    Hack to ensure optimizer variables have been created.

    This is normally run on the first optimization step, but various tests save before
    running a single step. If the optimizer state is not stored in a checkpoint then
    loading from that checkpoint won't reset the optimizer state to default.
    """
    optimizer._create_slots(model_weights)
    optimizer._create_hypers()
    optimizer.iterations


def _deserialize_loss_and_metric(
    loss_str: str,
) -> Tuple[tf.keras.losses.Loss, tf.keras.metrics.Metric]:
    """Get the loss and corresponding metric from string representation."""
    # we use weighted-mean losses/metrics
    if loss_str == "huber":
        loss = losses.WeightedHuber(reduction=tf.keras.losses.Reduction.SUM)
        metric = metrics_lib.MeanHuber(name="weighted_loss")
    elif loss_str == "mse":
        print("Warning: interpreting loss='mse' as SquaredDifference")
        loss = losses.SquaredDifference(reduction=tf.keras.losses.Reduction.SUM)
        metric = tf.keras.metrics.MeanSquaredError(name="weighted_loss")
    elif loss_str.startswith("pinball="):
        # If using Pinball loss, expects a string of form pinball=0.X (e.g. pinball=0.7)
        # where 0.X is the tau parameter
        _pinball, tau = loss_str.split("=")
        tau = float(tau)
        loss = losses.Pinball(reduction=tf.keras.losses.Reduction.SUM, tau=tau)
        metric = metrics_lib.MeanPinball(name="weighted_loss", tau=tau)
    else:
        raise ValueError("Wrong loss input! Must be huber, mse or pinball!")
    return loss, metric


def _get_optimizer(lr, grad_clip_value):
    opt_params = {"lr": lr}
    if grad_clip_value > 0:
        opt_params["clipvalue"] = grad_clip_value
    return tf.keras.optimizers.Adam(**opt_params)


class KerasModel(tf.keras.Model):
    def build_and_compile(self, inputs):
        """ Builds and compiles the inference model using symbolic inputs.

        Uses implementation of `_get_loss_optimizer_metric` utility function.

        Args:
            inputs: inputs to the inference model e.g.,
            the result of training_pipeline.inputs()
        """
        # == Build the model ===
        # Keras allows models to be constructed lazily (i.e., the shape
        # of the weights are determined only when the first input is passed).
        # Here, we pass a symbolic input to make this happen.
        # In principle, keras_model.build(input_shape) should be able to do
        # this but it has the limitation that it doesn't respect data types.
        self.call(inputs)

        loss, optimizer, metric = self._get_loss_optimizer_metric()

        # Build the optimizer
        _init_optimizer_vars(optimizer, self.trainable_weights)

        # == Attach loss, metric, optimizer to the model ==
        # Note that we are not directly using keras's `fit()` function
        # so the loss and metrics are not strictly needed. So subclasses
        # may provide them to the train/test functions in a different way.
        self.compile(
            loss=loss, optimizer=optimizer, metrics=[metric, metrics_lib.TotalWeight()],
        )

    @abc.abstractmethod
    def _get_loss_optimizer_metric(self):
        """Returns the loss, optimizer, and metric to be used in build_and_compile. """
        raise NotImplementedError

    @abc.abstractmethod
    def call(self, inputs):
        """ Implementation of the forward pass.

        Args:
            inputs: inputs to the inference model, e.g.,
            the result of training_pipeline.inputs()
        """
        raise NotImplementedError

    @abc.abstractmethod
    def train_on_batch(self, *args):
        """ Implementation of a training step on a single minibatch.

        Args:
            variable number of input (tensors) corresponding to
            training_pipeline.batched_spec.
        """
        raise NotImplementedError

    @abc.abstractmethod
    def test_on_batch(self, *args):
        """ Implementation of the evaluation of loss and metrics on a single minibatch.

        Args:
            variable number of input (tensors) corresponding to
            training_pipeline.batched_spec.
        """
        raise NotImplementedError


def tf_function_unless_eager(func, spec, eager):
    return func if eager else tf.function(func, spec)


class ModelWrapper:
    """
    Keras Model wrapper that allows both eager and non-eager execution.

    This class implements methods to deal with the model, e.g., `train_on_batch`,
    `evaluate_loss`, and `evaluate_all_time_left`, as well as methods to deal
    with the pipeline, e.g., `create_dataset`.
    """

    def __init__(
        self,
        seed: int,
        training_pipeline: TrainingPipeline,
        keras_model: KerasModel,
        num_time_heads: int,
        device: Optional[str] = None,
        eager: bool = False,
    ):
        self._graph_pipeline = training_pipeline._graph_pipeline
        self._training_pipeline = training_pipeline
        self._num_time_heads = num_time_heads
        tf.random.set_seed(seed)
        with utils.random_state_context(
            seed
        ), utils.nullcontext() if device is None else tf.device(device):
            keras_model.build_and_compile(self._training_pipeline.inputs())
            self._keras_model = keras_model
            self._keras_model.run_eagerly = eager
            # self._chkpt = tf.train.Checkpoint(model=self._keras_model)
            spec = self._training_pipeline.batched_spec  # inputs, labels, and weights
            self._train_on_batch = tf_function_unless_eager(
                keras_model.train_on_batch, spec, eager
            )
            self._test_on_batch = tf_function_unless_eager(
                keras_model.test_on_batch, spec, eager
            )
            self._evaluate_all_time_left = tf_function_unless_eager(
                self._keras_model.__call__, (self._graph_pipeline.batched_spec,), eager
            )

    @property
    def num_time_heads(self) -> int:
        return self._num_time_heads

    def reset_metrics(self):
        for m in self._keras_model.compiled_metrics._metrics:
            m.reset_states()

    def metric_results(self) -> Dict[str, float]:
        return {
            m.name: m.result().numpy()
            for m in self._keras_model.compiled_metrics._metrics
        }

    @property
    def keras_model(self) -> tf.keras.Model:
        return self._keras_model

    def prepare_batch(self, examples: Iterable[RawExample]):
        """Convert (time, expression, target) examples into a single batch of inputs."""
        return self._training_pipeline.prepare_batch(examples)

    def create_dataset(
        self,
        examples: Iterable[RawExample],
        shuffle_rng=None,
        max_nodes: Optional[int] = None,
        batch_size: Optional[int] = None,
    ) -> tf.data.Dataset:
        return self._training_pipeline.as_dataset(
            examples,
            shuffle_rng=shuffle_rng,
            max_nodes=max_nodes,
            batch_size=batch_size,
        )

    def evaluate_in_batches(
        self,
        examples: Iterable[ExprWithEnv],
        max_nodes: Optional[int] = None,
        batch_size: Optional[int] = None,
    ) -> Union[List[np.ndarray], List["RawPolicyValueEvaluation"]]:
        """ Evaluates a series of Expressions.
            Equivalent to calling evaluate_all_time_left on the Expressions after dividing into batches.
            Returns a 2d array - the outer index identifying the Expression, the inner index being the time_left.
        """
        # Pass all the examples to the graph pipeline for it to batch up their graph representations.
        # The islice gets us the corresponding ExprWithEnvs for each batch, so we can denormalize costs.
        example_iter = iter(examples)
        return [
            evaluation
            for batch in self._graph_pipeline.as_dataset(
                examples, max_nodes=max_nodes, batch_size=batch_size
            )
            for evaluation in self._evaluate_prepped(
                list(islice(example_iter, 0, len(batch))), batch
            )
        ]

    def train_on_batch(self, dataset_batch) -> float:
        """
        Train on a preprocessed batch.

        Performs the optimizer update and returns the loss with training=True.
        """
        dataset_batch = tf.nest.map_structure(tf.convert_to_tensor, dataset_batch)
        return self._train_on_batch(*dataset_batch).numpy()

    def evaluate_loss(self, dataset_batch) -> float:
        """
        Get the loss on a preprocessed batch.

        Runs in non-training mode and does not perform any optimizer updates.
        """
        dataset_batch = tf.nest.map_structure(tf.convert_to_tensor, dataset_batch)
        return self._test_on_batch(*dataset_batch).numpy()

    def __enter__(self):
        return self

    def __exit__(self, ex_type, exc, ex_trace):
        return False  # Do not suppress any exception

    def load_weights(self, path) -> None:
        print("Restoring weights from %s." % path)
        self.set_weights(Weights.from_path(path))

    def _weights(self) -> Tuple[tf.Variable]:
        return tuple(self._keras_model.weights) + tuple(
            self._keras_model.optimizer.weights
        )

    def set_weights(self, weights: "Weights") -> None:
        src_weights = weights.weights
        dst_weights = self._weights()
        assert len(src_weights) == len(dst_weights)
        for src, dst in zip(src_weights, dst_weights):
            dst.assign(src)

    def get_weights(self) -> "Weights":
        return Weights(tuple(w.numpy() for w in self._weights()))

    def distill_model(self, distillator, data, seed):
        last_weights = distillator(model_wrapper=self, seed=seed, dataset=data)
        return last_weights

    def evaluate_all_time_left(self, expr_batch: Iterable[ExprWithEnv]) -> np.ndarray:
        """ Given a list of expressions, returns a list containing the result of
            model evaluated on each expression for every time_left.
        """
        return self._evaluate_prepped(
            expr_batch, self._graph_pipeline.prepare_batch(expr_batch),
        )

    def _evaluate_prepped(self, expr_batch: Iterable[ExprWithEnv], prepared_data):
        vals = self._evaluate_all_time_left(
            tf.nest.map_structure(tf.convert_to_tensor, prepared_data)
        )
        return self._training_pipeline.denormalize_values(vals.numpy(), expr_batch)

    def as_train_search_model(self) -> "ModelInterpreter":
        return ModelInterpreter(self)

    def as_eval_search_model(self) -> "ModelInterpreter":
        return ModelInterpreter(self)


class Weights:
    def __init__(self, weights: Tuple[np.ndarray, ...]):
        self._weights = weights

    @staticmethod
    def from_path(path) -> "Weights":
        with open(path, "rb") as f:
            with np.load(f, encoding="bytes", allow_pickle=True) as data:
                weights_dict = data["weights"].item()
                return Weights(
                    tuple(
                        weights_dict["w{}".format(i)] for i in range(len(weights_dict))
                    )
                )

    @property
    def weights(self):
        return self._weights

    def save(self, path):
        print("Saving model to {}".format(path))
        with utils.open_file_mkdir(path, "wb") as f:
            np.savez(
                f, weights={"w{}".format(i): w for i, w in enumerate(self.weights)}
            )

    def request_distill(
        self, name_suffix, distillator, data, seed, urgency, time_acc, scope
    ):
        new_weights = yield worker.RunOnGPU(
            "distill_" + name_suffix,
            self,
            lambda model_wrapper: model_wrapper.distill_model(distillator, data, seed),
            urgency=urgency,
            time_acc=time_acc,
            scope=scope,
        )
        return new_weights


class DualModelWrapper(ModelWrapper):
    first_model_name = "model_1"
    second_model_name = "model_2"

    def __init__(
        self,
        model1: ModelWrapper,
        model2: ModelWrapper,
        var_fraction_train: float,
        var_fraction_eval: float,
    ):
        self.model1 = model1
        self.model2 = model2
        self.var_fraction_train = var_fraction_train
        self.var_fraction_eval = var_fraction_eval

        # check that models are initialised differently
        w1s = model1.get_weights().weights
        w2s = model2.get_weights().weights
        assert len(w1s) != len(w2s) or not all(
            np.array_equal(w1, w2) for w1, w2 in zip(w1s, w2s)
        )

    @staticmethod
    def seed_for_second_model(seed_for_first_model):
        # seed_for_first_model might be the repetition number (for the initial weights),
        # or a number drawn from an RNG (for e.g. shuffling in distillation).
        # Avoid any possibility of producing an out-of-range seed for a new RNG.
        return seed_for_first_model ^ 0xAAAAAAAA

    def distill_model(self, distillator, data, seed):
        # DualWeights.request_distill should route calls appropriately.
        raise NotImplementedError(
            "Do not call directly; use distill_{first,second}_model"
        )

    def distill_first_model(self, distillator, data, seed):
        with analytics.Scope(distill_net=self.first_model_name):
            return self.model1.distill_model(distillator, data, seed)

    def distill_second_model(self, distillator, data, seed):
        # Use a different seed to distill the second model to ensure different batching.
        with analytics.Scope(distill_net=self.second_model_name):
            return self.model2.distill_model(
                distillator, data, DualModelWrapper.seed_for_second_model(seed)
            )

    def as_train_search_model(self):
        return DualModelInterpolator(self, self.var_fraction_train)

    def as_eval_search_model(self):
        return DualModelInterpolator(self, self.var_fraction_eval)

    def set_weights(self, weights: "Weights") -> None:
        self.model1.set_weights(weights.weights1)
        self.model2.set_weights(weights.weights2)

    def get_weights(self) -> "Weights":
        weights1 = Weights(tuple(w.numpy() for w in self.model1._weights()))
        weights2 = Weights(tuple(w.numpy() for w in self.model2._weights()))
        return DualWeights(weights1, weights2)


class ModelInterpreter:
    def __init__(self, model: ModelWrapper):
        self.model = model

    def evaluate_all_time_left(self, expr_batch: Iterable[Expression]):
        return self.model.evaluate_all_time_left(expr_batch)


class DualModelInterpolator(ModelInterpreter):
    def __init__(self, model: DualModelWrapper, var_fraction: float):
        self.model1 = model.model1
        self.model2 = model.model2
        self.var_fraction = var_fraction

    def evaluate_all_time_left(self, expr_batch: Iterable[Expression]):
        v1 = self.model1.evaluate_all_time_left(expr_batch)
        v2 = self.model2.evaluate_all_time_left(expr_batch)
        return (v1 + v2) / 2 + self.var_fraction * np.abs(v1 - v2)


class DualWeights(Weights):
    def __init__(self, weights1: Weights, weights2: Weights):
        self.weights1 = weights1
        self.weights2 = weights2

    def save(self, path):
        path_before_ext, ext = path.split(".")
        self.weights1.save(path_before_ext + "_1" + ext)
        self.weights2.save(path_before_ext + "_2" + ext)

    def request_distill(
        self, name_suffix, distillator, data, seed, urgency, time_acc, scope
    ):
        new_weights_1, new_weights_2 = yield worker.RunMultiple(
            [
                worker.RunOnGPU(
                    "distill1_" + name_suffix,
                    self,
                    lambda model_wrapper: model_wrapper.distill_first_model(
                        distillator, data, seed
                    ),
                    urgency=urgency,
                    time_acc=time_acc,
                    scope=scope,
                ),
                worker.RunOnGPU(
                    "distill2_" + name_suffix,
                    self,
                    lambda model_wrapper: model_wrapper.distill_second_model(
                        distillator, data, seed
                    ),
                    urgency=urgency,
                    time_acc=time_acc,
                    scope=scope,
                ),
            ]
        )
        return DualWeights(new_weights_1, new_weights_2)

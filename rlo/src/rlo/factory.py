import functools
import os
from typing import Any, Callable, Dict, Literal, Optional, Sequence, Tuple

import numpy as np
import tensorflow as tf
import torch
import GPUtil


from rlo import rewrites, search_tree, utils
from rlo.best_results import TypeBestCost
from rlo.config_utils import kwargs_from_config
from rlo.expr_sets import ExpressionSet, get_expression_set
from rlo.expression import Expression

from rlo.model.layers import global_pooling_from_str
from rlo.model.model import TorchModelWrapper, Model, ModelState
from rlo.model.state_value_estimator import StateValueModel
from rlo.pipelines.graph_pipeline import EdgeType
from rlo.expression_util import NamedExprWithEnv
from rlo.search_ops import AbstractSearcher
from rlo.tf_model import ModelWrapper, DualModelWrapper, Weights
from rlo.torch_graph_data import DataConverter
from rlo.training import TrainingConfig, train_model


ConfigType = Dict[str, Any]


def get_train_and_eval_exprs(
    config: ConfigType,
) -> Tuple[ExpressionSet, ExpressionSet]:
    """Return a tuple of
    (1) the training expressions
    (2) the evaluation expressions, i.e. test + train
    """
    train_exprs = get_expression_set(
        config["train_exprs"], take_defs=config.get("train_on_defs")
    )
    test_exprs = get_expression_set(
        config["test_exprs"], take_defs=config.get("test_on_defs")
    )

    # Take a union so that we evaluate on both train and test expressions
    # using the same searcher. Success metrics are calculated and plotted
    # separatedly.
    return train_exprs, train_exprs.union(test_exprs)


def alpha_schedule_from_config(config: ConfigType):
    from rlo.rollouts import AlphaSchedule

    kwargs = dict(
        init_alpha=1.0,
        max_alpha=10.0,
        alpha_scaling_factor=1.1,
        alpha_scaling_factor_fail=1.0,
    )
    kwargs.update(
        {
            k: config[k]
            for k in (
                "init_alpha",
                "max_alpha",
                "alpha_scaling_factor",
                "alpha_scaling_factor_fail",
            )
            if k in config
        }
    )
    return AlphaSchedule(
        init_value=kwargs["init_alpha"],
        max_value=kwargs["max_alpha"],
        scale_factor_success=kwargs["alpha_scaling_factor"],
        scale_factor_fail=kwargs["alpha_scaling_factor_fail"],
    )


PhaseType = Literal["train", "eval"]


def rollout_searcher_from_config(
    config: ConfigType, phase: PhaseType
) -> AbstractSearcher:
    required = (
        "num_episode_clusters",
        "num_positive_examples",
        "alpha_test",
    )
    from rlo.rollouts import ValueBasedRolloutSearcher

    optional = ("batch_size",)
    renames = (
        ("search_batch_size", "batch_size"),
        (f"max_num_episodes_{phase}", "max_num_episodes"),
        (f"simulation_depth_{phase}", "simulation_depth"),
    )
    kwargs = kwargs_from_config(config, required, optional, renames)
    if "maxing" in config:
        kwargs["maxing"] = maxing_algs[config["maxing"]]
    return ValueBasedRolloutSearcher(
        rules=rewrites.get_rules(config["rules"]),
        alpha_schedule=alpha_schedule_from_config(config),
        **kwargs,
    )


def searcher_kwargs_from_config(config: ConfigType, phase: PhaseType) -> Dict[str, Any]:
    """ Arguments general to most searchers, with the exception of Rollout. """
    required = ()
    optional = (
        "num_episode_clusters",
        "cost_per_step",
    )
    renames = (
        ("search_batch_size", "batch_size"),
        (f"max_gnn_{phase}", "max_gnn"),
        (f"simulation_depth_{phase}", "simulation_depth"),
    )
    kwargs = kwargs_from_config(config, required, optional, renames)
    if "maxing" in config:
        kwargs["maxing"] = maxing_algs[config["maxing"]]
    kwargs["rules"] = rewrites.get_rules(config["rules"])
    return kwargs


def astar_searcher_from_config(
    config: ConfigType, phase: PhaseType
) -> AbstractSearcher:
    from rlo.astar_search import AStarSearcher

    return AStarSearcher(**searcher_kwargs_from_config(config, phase))


def beam_searcher_from_config(config: ConfigType, phase: PhaseType) -> AbstractSearcher:
    from rlo.beam_search import PseudoBeamSearcher

    return PseudoBeamSearcher(**searcher_kwargs_from_config(config, phase))


def hybrid_searcher_from_config(
    config: ConfigType, phase: PhaseType
) -> AbstractSearcher:
    from rlo.hybrid_search import HybridSearcher, MergeHandling

    required = ()
    optional = ()
    renames = (
        ("hybrid_prob_rollout", "prob_rollout"),
        ("hybrid_alpha", "alpha"),
    )
    kwargs = {
        **searcher_kwargs_from_config(config, phase),
        **kwargs_from_config(config, required, optional, renames),
    }
    if "hybrid_merge_handling" in config:
        kwargs["merge_handling"] = MergeHandling[config["hybrid_merge_handling"]]
    return HybridSearcher(**kwargs)


def searcher_from_config(
    config: ConfigType, phase: PhaseType = "train"
) -> AbstractSearcher:
    search = config[f"{phase}_search"]
    return search_algs[search](config, phase)


def tf_distillator_from_config(config):
    from rlo.distillator import Distillator
    from rlo.distillator_v2 import DistillatorV2

    kwargs = kwargs_from_config(
        config, ("max_epochs", "min_epochs", "patience_epochs"), ("split", "verbose"),
    )
    sparse = config.get("sparse_gnn", False)
    if sparse:
        kwargs["max_nodes_per_batch"] = config["max_nodes_per_batch"]
    else:
        kwargs["batch_size"] = config.get("batch_size", 16)
    if config.get("v2", False):
        kwargs["tb_dir"] = config.get("tb_dir")
        return DistillatorV2(**kwargs)
    else:
        if "verbose" in kwargs:
            del kwargs["verbose"]
        return Distillator(**kwargs)


def distillator_from_config(config):
    if config["tensorflow"]:
        return tf_distillator_from_config(config)

    # Pytorch. Sparse GNN only, so always batch by num_nodes.
    kwargs = kwargs_from_config(
        config,
        ("max_epochs", "min_epochs", "max_nodes_per_batch", "patience_epochs"),
        ("split",),
    )
    training_config = TrainingConfig(**kwargs)
    return functools.partial(train_model, config=training_config)


def dataset_refiner_factory_from_config(config: ConfigType):
    from rlo import dataset_refiner

    return dataset_refiner.construct_dataset_refiner_factory(
        [
            dataset_refiner.get_dataset_refiner_creator_by_name(class_name)
            for class_name in config["dataset_refiners"]
        ]
    )


def model_save_dir_from_config(config: ConfigType) -> str:
    # Support legacy configs with separate model_save_path
    return config.get(
        "model_save_path", os.path.join(config["result_save_path"], "saved_models")
    )


def rng_factory_from_config(
    config: ConfigType,
) -> Callable[[int], np.random.Generator]:
    def generation_rng(seed: int, generation: int) -> np.random.Generator:
        root_rng = utils.rng(seed)
        for _ in range(generation):
            # Skip generations. If this becomes expensive, we could consider
            # something like utils.rng(nth_prime(seed == repetition) * (generation+1))
            utils.seed(root_rng)
        return utils.rng(root_rng)

    return functools.partial(generation_rng, seed_from_config(config))


def expression_shuffler_from_config(
    config: ConfigType, train_exprs: Sequence[NamedExprWithEnv]
):
    from rlo.expr_shuffler import ExpressionShuffler

    return ExpressionShuffler(
        rng_factory_from_config(config), train_exprs, config["exprs_per_generation"]
    )


def best_cost_from_config(config):
    """ Returns a function that returns the best oracle or declared cost
        for a given expression taking into account time_budget if applicable.
        The function will first try to look up the oracle results in best_results.py.
        If that fails, it will try to look up the best costs declared in 
        train / test expression files. If that fails too, None is returned.
    """
    rules_name = config["rules"]
    _, eval_set = get_train_and_eval_exprs(config)

    @functools.lru_cache(maxsize=None)
    def best_cost_func(expr: str, time_budget: int) -> TypeBestCost:
        res = eval_set.best_cost_for_expression(expr, rules_name, time_budget)
        if res is None:
            raise ValueError(
                f"Could not find the best cost for {expr} at time budget of {time_budget}"
            )
        return res

    return best_cost_func


def azureml_reporter_from_config(config, train_set: ExpressionSet):
    from rlo.reporting.azureml import AzuremlReporter
    from rlo.reporting.utils import SearchOptimalityMetricFunction

    return AzuremlReporter(
        config["repetition"],
        config["num_repetitions"],
        SearchOptimalityMetricFunction(best_cost_from_config(config)),
        [name for name, _ in train_set.named_exprenvs()],
    )


def simul_search_curriculum_from_config(
    config: ConfigType, train_set: ExpressionSet, eval_set: ExpressionSet,
):
    from rlo.simul_search import SimultaneousSearchCurriculum

    return SimultaneousSearchCurriculum(
        train_exprs_per_generation=expression_shuffler_from_config(
            config, train_set.named_exprenvs()
        ),
        train_searcher=searcher_from_config(config, "train"),
        eval_expressions=eval_set.named_exprenvs(),
        eval_searcher=searcher_from_config(config, "eval"),
        distillator=distillator_from_config(config),
        dataset_refiner_factory=dataset_refiner_factory_from_config(config),
        rng_for_gen_fn=rng_factory_from_config(config),
        model_save_dir=model_save_dir_from_config(config),
        reporter=azureml_reporter_from_config(config, train_set),
        model_state_factory=ModelStateFactory.from_config(config),
        **{
            k: config[k]
            for k in (
                "num_generations",
                "total_train_time",
                "save_all_models",
                "repetition",
            )
            if k in config
        },
    )


class NoGPUError(ValueError):
    """Raise this when user demands GPU but there is no GPU."""


def torch_device_from_config(config: ConfigType) -> torch.device:
    """Get the CPU or GPU device to run on.  Optionally, also set the fraction of GPU memory to be used."""
    force_gpu = config.get("force_gpu", False)
    force_cpu = config.get("force_cpu", False)

    if force_cpu and force_gpu:
        raise ValueError("You cannot force both CPU and GPU")

    if (not force_cpu) and torch.cuda.is_available():
        # Default to GPU when available
        memory_frac = config.get("gpu_memory_fraction")
        if memory_frac is not None:
            torch.cuda.set_per_process_memory_fraction(memory_frac)
        return torch.device("cuda:0")
    else:
        if force_gpu:
            raise NoGPUError()
        return torch.device("cpu")


def tf_device_name_from_config(config: Dict) -> Optional[str]:
    """Get GPU device name, or None if GPU is not requested. Optionally, also set the fraction of GPU memory to be used."""
    force_gpu = config.get("force_gpu", False)
    if config.get("force_cpu", False):
        raise NotImplementedError
    if not force_gpu:
        return None
    physical_devices = tf.config.list_physical_devices("GPU")
    if len(physical_devices) == 0:
        raise NoGPUError("No GPUs visible - cannot use force_gpu")
    physical_device = physical_devices[0]
    memory_frac = config.get("gpu_memory_fraction")
    if memory_frac is not None:
        total_memory = GPUtil.getGPUs()[0].memoryTotal
        memory_limit = memory_frac * total_memory
        tf.config.set_logical_device_configuration(
            physical_device, [tf.config.LogicalDeviceConfiguration(memory_limit)]
        )
    return tf.config.list_logical_devices("GPU")[0].name


def num_time_heads_from_config(config: ConfigType):
    # At the moment, this is the only configuration we support,
    # although we expect these to diverge in the future.
    return config["simulation_depth_train"] + 1


def data_converter_from_config(config: ConfigType) -> DataConverter:
    kwargs = kwargs_from_config(
        config, required_keys=("use_subtree_match_edges",), optional_keys=(),
    )
    kwargs["num_time_heads"] = num_time_heads_from_config(config)
    kwargs["cost_norm"] = config["cost_normalization"]
    kwargs["device"] = torch_device_from_config(config)
    return DataConverter(**kwargs)


def torch_model_from_config(config: ConfigType, seed: Optional[int] = None) -> Model:
    # TODO edit flags so we don't need all these renames.
    model_kwargs = kwargs_from_config(
        config,
        required_keys=("num_gnn_blocks", "loss", "lr", "aggregation_over_edge_types"),
        optional_keys=("num_propagations",),
        renames=[
            ("hidden_dim", "gnn_hidden_dim"),
            ("output_hidden_dim", "regressor_hidden_dim"),
            ("decoder_readout", "global_pooling"),
        ],
    )
    global_pooling = model_kwargs.pop("global_pooling")
    model_kwargs["global_pooling"] = global_pooling_from_str(global_pooling)
    model_kwargs["gnn_dropout"] = 1 - config["graph_state_keep_prob"]
    model_kwargs["regressor_dropout"] = 1 - config["output_keep_prob"]
    model_kwargs["num_time_heads"] = num_time_heads_from_config(config)

    model_kwargs["num_node_types"] = Expression.num_node_types
    model_kwargs["num_edge_types"] = EdgeType.num_edge_types(
        use_subtree_match_edges=config["use_subtree_match_edges"]
    )
    seed = seed_from_config(config) if seed is None else seed
    with utils.random_state_context(seed):
        model = StateValueModel(**model_kwargs)
    return model


def tf_single_regressor_from_config(seed, config: ConfigType) -> ModelWrapper:
    from rlo.cost_normalizers import cost_normalizer
    from rlo.regressor import DenseGNNStateValueRegressor, SparseGNNStateValueRegressor

    sparse = config.get("sparse_gnn", False)
    cost_norm_str: str = config["cost_normalization"]
    cost_norm = cost_normalizer(cost_norm_str)

    # Model parameters go into model_hypers
    model_hypers = kwargs_from_config(
        config,
        ("output_hidden_dim", "num_propagations", "hidden_dim",),
        (
            "lr",
            "loss",
            "cumsum",
            "one_hot_embedding",
            "nonlinear_messages",
            "decoder_readout",
            "aggregation_over_edge_types",
            "message_from_sender_receiver",
            "grad_clip_value",
        ),
    )
    # All other parameters including pipeline parameters go into kwargs
    kwargs = kwargs_from_config(config, ("use_subtree_match_edges",), ("eager",),)
    kwargs["device"] = tf_device_name_from_config(config)
    kwargs["cost_norm"] = cost_norm
    kwargs["seed"] = seed_from_config(config) if seed is None else seed

    kwargs["num_time_heads"] = num_time_heads_from_config(config)
    if "graph_state_keep_prob" in config:
        model_hypers["recurrent_dropout"] = 1 - config["graph_state_keep_prob"]
    if "output_keep_prob" in config:
        model_hypers["output_dropout"] = 1 - config["output_keep_prob"]
    model_hypers["num_gnn_blocks"] = config["num_gnn_blocks"]
    model_hypers["stacked_gnn_double_hidden"] = config["stacked_gnn_double_hidden"]
    kwargs["model_hypers"] = model_hypers

    if sparse:
        return SparseGNNStateValueRegressor(**kwargs)
    else:
        return DenseGNNStateValueRegressor(**kwargs)


def single_regressor_from_config(config: ConfigType, seed=None):

    if config["tensorflow"]:
        return tf_single_regressor_from_config(seed=seed, config=config)

    data_converter = data_converter_from_config(config)
    model = torch_model_from_config(config)
    device = torch_device_from_config(config)
    return TorchModelWrapper(model=model, data_converter=data_converter, device=device)


def regressor_from_config(config: ConfigType):

    if config.get("two_value_func") is not None:
        if not config["tensorflow"]:
            raise NotImplementedError(
                "DualModelWrapper is not implemented for torch models."
            )
        # Store seed_2 in config just so we have a record.
        first_seed = seed_from_config(config)
        second_seed = DualModelWrapper.seed_for_second_model(first_seed)
        config["seed_2"] = second_seed
        model_1 = single_regressor_from_config(config, first_seed)
        model_2 = single_regressor_from_config(config, second_seed)

        if config["two_value_func"] == "train":
            return DualModelWrapper(
                model_1, model_2, config["two_value_func_var_frac_train"], 0
            )
        else:
            raise ValueError("For two value functions only train is supported atm.")
    else:
        return single_regressor_from_config(config)


search_algs = {
    "rollout": rollout_searcher_from_config,
    "astar": astar_searcher_from_config,
    "beam": beam_searcher_from_config,
    "hybrid": hybrid_searcher_from_config,
}


maxing_algs = {
    "episodes": search_tree.max_over_episodes,
    "accumulator": search_tree.max_with_accumulator,
}


def seed_from_config(config: ConfigType):
    return int(
        config[
            "seed_all_reps" if config.get("seed_all_reps") is not None else "repetition"
        ]
    )


def framework_from_config(config: ConfigType):
    # Field tensorflow doesn't exist before PyTorch port
    tensorflow = config.get("tensorflow", True)
    return "tensorflow" if tensorflow else "torch"


class ModelStateFactory:
    def __init__(self, model_state_class):
        self._model_state_class = model_state_class

    @staticmethod
    def from_config(config: ConfigType):
        framework = framework_from_config(config)
        return ModelStateFactory(Weights if framework == "tensorflow" else ModelState)

    def from_path(self, path):
        return self._model_state_class.from_path(path)

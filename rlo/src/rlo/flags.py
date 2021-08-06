import argparse
import json
import os
import pickle
import sys
import time
from typing import Iterable, List, Dict

from rlo import config_utils
from rlo.cost_normalizers import available_cost_normalizers
from rlo.dataset_refiner import get_available_dataset_refiners
from rlo import factory
from rlo.extra_plots import extra_plots
from rlo.hybrid_search import MergeHandling
from rlo import utils
from rlo import git_utils
from rlo.utils import FunctionArgs as Args
from rlo.layers import available_aggregations
from rlo.expression_util import NamedExprWithEnv
from rlo.factory import ConfigType


def loss_str(loss: str) -> str:
    # Validator for command-line 'loss' argument
    if loss in ["huber", "mse"]:
        return loss
    if loss.startswith("pinball="):
        _pinball, tau = loss.split("=")
        if not 0.0 <= float(tau) <= 1.0:
            raise ValueError("Tau must be between 0 and 1")
        return loss
    raise ValueError


# Note there is no way to specify train_exprs, test_exprs or rules here
# - they must come from the scenario or replay_config
general_arguments = [
    Args(
        "--exprs_per_generation",
        type=int,
        default=0,
        help="Frequency with which to perform evaluation, once per this many training exprs",
    ),
    Args(
        "--no_subtree_match_edges",
        action="store_false",
        dest="use_subtree_match_edges",
        help="Do not use connect identical sub expressions with a special edge type",
    ),
    Args(
        "--num_propagations",
        type=int,
        default=10,
        help="number of the steps of the dynamics of GNN",
    ),
    Args(
        "--nonlinear_messages",
        action="store_true",
        help="If True, apply nonlinearity before edge-to-vertex message aggregation.",
    ),
    Args(
        "--aggregation_over_edge_types",  # TODO concatenate and concatenate_by_agg_type not available in torch (yet)
        type=str,
        default="sum",
        choices=available_aggregations,
        help="aggregation of all edge_type messages before passing into gru",
    ),
    Args(
        "--decoder_readout",
        type=str,
        choices=["mean", "max", "min", "sum"],
        default="mean",
        help="How do we aggregate node features before feeding them to the value function.",
    ),
    Args(
        "--message_from_sender_receiver",  # TODO: Not yet implemented in torch
        action="store_true",
        help="If true, the message is computed both using the sender and the receiver features.",
    ),
    Args(
        "--one_hot_embedding",  # TODO: Not yet configurable in torch implementation
        action="store_true",
        help="use one-hot initial node embedding rather than learned lookup",
    ),
    Args("--hidden_dim", type=int, default=200, help="GNN hidden dimension"),
    Args(
        "--output_hidden_dim", type=int, default=200, help="output MLP hidden dimension"
    ),
    Args("--gamma", type=float, default=0.1, help="discount factor (not used)"),
    Args(
        "--max_num_episodes_train",
        type=int,
        default=4 ** 6,
        help="max number of simulation episodes during train search",
    ),
    Args(
        "--max_num_episodes_eval",
        type=int,
        default=100,
        help="max number of simulation episodes during eval search",
    ),
    Args(
        "--num_positive_examples",
        type=int,
        default=10,
        help="min number of positive episodes",
    ),
    Args(
        "--simulation_depth_train",
        type=int,
        default=None,
        help="max depth to simulate during training",
    ),
    Args(
        "--simulation_depth_eval",
        type=int,
        default=None,
        help="max depth to simulate during evaluation",
    ),
    Args(
        "--maxing",
        type=str,
        help="algorithm to extract best empirical optimizations from the search tree",
        default="accumulator",
        choices=factory.maxing_algs.keys(),
    ),
    Args(
        "--min_epochs",
        type=int,
        default=10,
        help="Min number of training epochs per generation (looking for epoch with lowest validation loss)",
    ),
    Args(
        "--max_epochs",
        type=int,
        default=None,
        help="max number of training epochs per generation (default = limited by patience only)",
    ),
    Args(
        "--num_repetitions", type=int, default=8, help="number of training repetitions"
    ),
    Args(
        "--graph_state_keep_prob",
        type=float,
        default=0.5,
        help="dropout keep probability for graph state",
    ),
    Args(
        "--output_keep_prob",
        type=float,
        default=0.5,
        help="dropout keep probability for output MLP",
    ),
    Args("--cost_normalization", type=str, choices=available_cost_normalizers()),
    Args(
        "--patience_epochs",
        type=int,
        default=4,
        help="stop training if validation has not improved in this number of epochs",
    ),
    Args(
        "--num_generations",
        type=int,
        default=None,
        help="number of generations over expressions",
    ),
    Args(
        "--total_train_time",
        type=int,
        default=None,
        help="stop at end of first generation when training time exceeds this (seconds)",
    ),
    Args(
        "--num_episode_clusters",
        type=int,
        default=5,
        help="number of clusters of episodes used to build dataset",
    ),
    Args(
        "--template_path",
        type=str,
        help="Path to template .kso file (from rlo directory), e.g. ksc/blas/blas_template.kso",
    ),
    Args(
        "--test_on_defs",
        type=str,
        default=None,
        nargs="+",
        help="allows to choose which functions from the test set to use",
    ),
    Args(
        "--train_on_defs",
        type=str,
        default=None,
        nargs="+",
        help="allows to choose which functions from the train set to use",
    ),
    Args(
        "--seed_all_reps",
        type=int,
        help="Seed all repetitions with same seed (usually each rep seeded with its number)",
    ),
    Args(
        "--loss",
        type=loss_str,
        default="huber",
        help="types of the losses available.  Options are 'huber', 'mse', or "
        "strings of the form 'pinball=0.9' where 0.9 is the tau parameter for pinball loss.",
    ),
    Args("--lr", type=float, default=0.0001, help="learning rate"),
    Args(
        "--grad_clip_value",
        type=float,
        default=0,
        help="Coefficient used for gradient clipping by value. "
        "If <=0 (default), the gradients will not be clipped. ",
    ),
    Args("--split", type=float, default=0.9, help="train-validation split parameter"),
    Args(
        "--value_bin_splits",
        type=float,
        nargs="+",
        default=None,
        help="bin splits to use for value distribution plot",
    ),
    Args(
        "--time_bin_splits",
        type=int,
        nargs="+",
        default=None,
        help="bin splits to use for time distribution plot",
    ),
    Args(
        "--episode_bin_splits",
        type=int,
        nargs="+",
        default=None,
        help="bin splits to use for episode distribution plot",
    ),
    Args(
        "--extra_plots",
        type=str,
        nargs="*",
        default=[],
        choices=extra_plots.keys(),
        help="Extra plots",
    ),
    Args("--v2", action="store_true", help="use distillator_v2"),
    Args("--verbose", action="store_true", help="use distillator_v2 verbose"),
]


dataset_processing_arguments = [
    Args(
        "--dataset_refiners",
        nargs="*",
        type=str,
        default=["best_across_generations_refiner"],
        choices=get_available_dataset_refiners(),
        help="Sequence of dataset refiners to use",
    ),
]

search_algorithm_arguments = [
    # The first two will need to be specified somewhere, probably in scenario
    Args(
        "--train_search",
        type=str,
        choices=factory.search_algs.keys(),
        help="Search algorithm for training.",
    ),
    Args(
        "--eval_search",
        type=str,
        choices=factory.search_algs.keys(),
        help="Search algorithm for evaluation.",
    ),
    Args(
        "--cost_per_step",
        type=float,
        default=None,
        help="Use cost_per_step in search (0 = just take max over t'<t), for A*/Hybrid/Beam only",
    ),
    Args(
        "--max_gnn_train",
        type=int,
        default=None,
        help="Max GNN evaluations in training; for A*/Hybrid/Beam only",
    ),
    Args(
        "--max_gnn_eval",
        type=int,
        default=None,
        help="Max GNN evaluations in test; for A*/Hybrid/Beam only",
    ),
    Args(
        "--search_batch_size",
        type=int,
        default=16,
        help="Batch size to use for GNN evaluation during search",
    ),
    Args(
        "--hybrid_merge_handling",
        type=str.upper,
        default=MergeHandling.STOP.name,
        choices=[m.name for m in MergeHandling],
    ),  # Help string shows uppercase, but parsed insensitively
    Args(
        "--hybrid_prob_rollout",
        type=float,
        default=1.0,
        help="Probability of rolling out one more step in hybrid search",
    ),
    Args(
        "--hybrid_alpha",
        type=float,
        default=float("inf"),
        help="Alpha value for hybrid search",
    ),
]

# alpha_tuning_arguments are only applicable for the rollout search algorithm
alpha_tuning_arguments = [
    Args(
        "--alpha_test",
        type=float,
        default=5.0,
        help="Temperature for test runs. This does NOT affect the alpha for training runs.",
    ),
    Args(
        "--init_alpha",
        type=float,
        default=1.0,
        help="Temperature for train runs at the first generation. Used in softmax action selection.",
    ),
    Args(
        "--alpha_scaling_factor",
        type=float,
        default=1.1,
        help="Alpha for training is multiplied by it on success.",
    ),
    Args(
        "--alpha_scaling_factor_fail",
        type=float,
        default=1.0,
        help="Alpha for training is multiplied by it on failure.",
    ),
]

sparse_gnn_arguments = [
    Args(
        "--sparse_gnn",
        action="store_true",
        help="For tensorflow, this flag enables sparse GNN. For Pytorch, sparse GNN is the only option, and this flag is ignored.",
    ),
    Args(
        "--tensorflow",
        action="store_true",
        dest="tensorflow",
        help="Use tensorflow implementation. If not specified, default to pytorch.",
    ),
    Args(
        "--num_gnn_blocks",
        type=int,
        default=1,
        help="How many GNN blocks to use. Should be a divider of the number of propagations. E.g. if we have --num_propagations=10 and 2 blocks, each block will do 5 propagations. "
        "Will use StackedGNNEncoder if set --num_gnn_blocks. Will use old (default) SparseGNNEncoder if set to 1.",
    ),
    Args(
        "--stacked_gnn_double_hidden",
        action="store_true",
        help="If set, each next GNN block of a stack will have its hidden dim doubled. "
        "Otherwise apply dimensionality reduction before the GNN output.",
    ),
    Args(
        "--max_nodes_per_batch",
        type=int,
        default=10000,
        help="Maximum number of nodes in a sparse GNN batch",
    ),
]

value_function_arguments = [
    # The nargs means that if no --cumsum is specified, returns the default; if cumsum is specified without argument, returns the 'const'.
    Args(
        "--cumsum",
        type=float,
        nargs="?",
        default=None,
        const="inf",
        help="Use cumulative sum, with optional alpha value for softplus (else INF => relu)",
    ),
    Args(
        "--two_value_func",
        type=str,
        default=None,
        choices=["train"],
        help="Whether to use two value functions for training phase",
    ),
    Args(
        "--two_value_func_var_frac_train",
        type=float,
        help="Fraction of variance to add to mean when merging two value functions during training phase.",
    ),
]

# Fields from legacy configs, and legacy arguments. These will be *rejected* if present.
# Note the "merge-datasets" value in config.json was controlled by command-line argument --no_merge_datasets.
config_argument_deprecations = {
    "num_timesteps": "num_propagations",
    "merge_datasets": None,
    "num_epochs": "num_generations",
    "max_depth": "simulation_depth_train",
    "total_num_iterations": "min_epochs/max_epochs",
    "patience": "patience_epochs",
    "nested": None,
    "max_num_episodes": "max_num_episodes_train",
    "simulation_depth": "simulation_depth_train and --simulation_depth_eval",
}

config_arguments = (
    general_arguments
    + dataset_processing_arguments
    + search_algorithm_arguments
    + alpha_tuning_arguments
    + sparse_gnn_arguments
    + value_function_arguments
    + [
        Args("--" + k, action="help", help="Do not use; use --{} instead".format(v))
        for k, v in config_argument_deprecations.items()
    ]
)

run_arguments = [
    Args(
        "scenario",
        type=str,
        help="Path to scenario or name of .json (potentially old config)",
    ),
    Args("--run_id", type=str, default=None, help="Run ID (best to keep default)"),
    Args("--gitlog", type=str, default=git_utils.get_git_revision_short_hash()),
    Args("--output_dir", type=str, default="outputs", help="Where models and plots go"),
    Args(
        "--repetition",
        type=int,
        default=None,
        help="run just a single repetition, without plotting (use with run_id)",
    ),
    Args(
        "--force_gpu",
        action="store_true",
        help="Raise exception if we cannot create model on GPU",
    ),
    Args(
        "--force_cpu",
        action="store_true",
        help="Run on CPU even if GPU is available (useful if CUDA nondeterminism is a problem). Only supported for PyTorch.",
    ),
    Args(
        "--gpu_memory_fraction",
        type=float,
        default=None,
        help="Use this fraction of GPU (e.g. 0.5) to allow sharing",
    ),
    Args(
        "--save_all_models",
        action="store_true",
        help="whether to store weights after every generation, or just to store them at the end of training",
    ),
    Args(
        "--num_parallel",
        type=int,
        default=1,
        help="number of parallel threads on which to run repetitions",
    ),
    Args(
        "--upload_models",
        action="store_true",
        help="Upload models into the Azure ML workspace",
    ),
]

run_arguments_no_parallel = [a for a in run_arguments if a.args[0] != "--num_parallel"]
assert len(run_arguments_no_parallel) + 1 == len(run_arguments)  # Check we removed it

ray_run_arguments = run_arguments_no_parallel + [
    Args(
        "--address",
        default=None,
        type=str,
        help="ip_address:port to connect to redis (default is to run internally)",
    ),
    Args("--redis_token", default=None, type=str, help="password to connect to redis"),
    Args(
        "--log_to_driver",
        action="store_true",
        help="Log all worker stdout/err to driver",
    ),
    Args(
        "--num_gpus",
        default=None,
        type=int,
        help="Override Ray autodetecting number of GPUs (for this node only)",
    ),
    Args(
        "--num_cpus",
        default=None,
        type=int,
        help="Override Ray autodetecting number of CPUs (for this node only)",
    ),
    Args(
        "--workers_per_gpu",
        default=1,
        type=int,
        help="Tell Ray to put this number of workers (cpu threads+models) on each GPU",
    ),
    Args(
        "--test_kill_worker_after_tasks",
        default=-1,
        type=int,
        help="Kill each remote worker after this many tasks (negative => never)",
    ),
    Args(
        "--ray_timeout",
        default=3600,
        type=int,
        help="Starting timeout for ray tasks; doubles after each failure to complete",
    ),
    Args(
        "--profile_local",
        nargs="?",
        type=int,
        default=0,
        help="Run headnode under cprofile, with optional limit/s on local task duration",
    ),
    Args(
        "--timeline",
        action="store_true",
        help="Produce ray_timeline.json and ray_object_transfers.json",
    ),
]


def make_parser(arguments: List[Args]) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    for args in arguments:
        utils.apply_args(parser.add_argument, args)
    return parser


def parse_with_defaults(argument_fields, defaults, cmdline=None, known=False) -> Dict:
    parser = make_parser(argument_fields)
    parser.set_defaults(**defaults)
    if known:
        args, _ = parser.parse_known_args(cmdline)
    else:
        args = parser.parse_args(cmdline)
    return vars(args)


def get_scenario_name_and_defaults(scenario_or_config):
    """ Given the name or path to a scenario .json, or the path to a previous experiment config,
        return the name of the underlying scenario, and the default values for all config_arguments
               and scenario-only fields """
    if not os.path.isfile(scenario_or_config):
        exec_dir = os.path.dirname(sys.argv[0])
        scenario_or_config = os.path.join(
            exec_dir, "scenarios", f"{scenario_or_config}.json"
        )
    with open(scenario_or_config) as f:
        scenario_dict = json.load(f)

    config_argument_fields = set(
        arg.args[0][2:] for arg in config_arguments if arg.args[0][:2] == "--"
    )
    scenario_required_fields = {"train_exprs", "test_exprs", "rules"}
    # When replaying a previous experiment, keep the overrides from that (as well as any new)
    scenario_optional_fields = {"oracle", "extra_scenario_params"}.union(
        config_argument_fields
    )

    scenario_overrides = config_utils.kwargs_from_config(
        scenario_dict,
        required_keys=scenario_required_fields,
        optional_keys=scenario_optional_fields,
    )
    if not scenario_overrides.keys().isdisjoint(config_argument_deprecations.keys()):
        raise ValueError(
            "\n".join(
                [
                    "Key {} has been {}".format(
                        k, "removed" if v is None else "replaced by {}".format(v)
                    )
                    for k, v in config_argument_deprecations.items()
                    if k in scenario_overrides
                ]
            )
        )

    # For running from a previous experiment, keep the old (base) scenario value
    scenario_name = scenario_dict.get(
        "scenario", os.path.splitext(os.path.basename(scenario_or_config))[0]
    )

    return (
        scenario_name,
        parse_with_defaults(config_arguments, scenario_overrides, cmdline=[]),
    )


def check_overrides(config: ConfigType, defaults: ConfigType):
    default_parser = make_parser(config_arguments)
    default_parser.set_defaults(**defaults)
    default_config = vars(default_parser.parse_args([]))  # ignore command line
    for k, v in default_config.items():
        if config[k] != v:
            print(
                "Overriding {} with value {} (original value={})".format(
                    k, config[k], v
                )
            )
            yield (k, config[k], v)


def make_config_for_scenario(
    scenario: str,
    run_args: List[Args] = run_arguments,
    cmdline: Iterable[str] = None,
    allow_unknown=False,
) -> ConfigType:
    """Create config for scenario"""
    # Now load scenario (/replayed config)
    scenario, scenario_defaults = get_scenario_name_and_defaults(scenario)
    return make_config(
        run_args + config_arguments, scenario, scenario_defaults, cmdline, allow_unknown
    )


def make_config(
    all_arguments, scenario, scenario_defaults, cmdline=None, allow_unknown=False
) -> ConfigType:
    # This time we'll parse all the arguments, but using scenario defaults; these are the values to actually use.
    config = parse_with_defaults(
        all_arguments, scenario_defaults, cmdline=cmdline, known=allow_unknown
    )

    config["scenario"] = scenario

    # Record those explicitly overridden on the commandline, in the description
    config["extra_scenario_params"] = "".join(
        [config.get("extra_scenario_params", "")]
        + [
            "+{}:{}".format(k, conf_v)
            for k, conf_v, _def_v in check_overrides(config, scenario_defaults)
        ]
    )

    # We've now computed global defaults, overridden by scenario, overridden by explicit cmdline.
    if config["run_id"] is None:
        config["run_id"] = "{}_{}_{}".format(
            config["scenario"], time.strftime("%Y_%m_%d_%H_%M_%S"), os.getpid()
        )
    assert config["run_id"] is not None
    config["result_save_path"] = os.path.join(
        config["output_dir"], "Run_" + config["run_id"]
    )
    if config["repetition"] is not None:
        # Apply the changes to directories etc. of the root config that we expect for the given repetition
        config = config_utils.config_for_repetition(
            # Base config = with repetition field removed
            {k: v for k, v in config.items() if k != "repetition"},
            config["repetition"],
        )
    return config


def _ensure_oracle_reachable(config, expressions: Iterable[NamedExprWithEnv]):
    """Check that all the expressions will have graphable known minimum costs.
    Although not essential (we can just not graph the minimum), usually it's
    helpful to know we *will* get a minumum on the graph before we spend hours running a test.
    Also updates the config's simulation_depth_train, simulation_depth_eval to ensure the
    run can reach the minimum."""
    from rlo import best_results

    for name, _ in expressions:
        # Use str to double-check it'll be reachable from logs
        assert best_results.best_cost_for_exp(name, config["rules"]).cost is not None

    # Also ensure we will be able to reach the optimal value.
    longest_sequence = max(
        [
            best_results.oracle_sequence(name, config["rules"])
            for name, _ in expressions
        ],
        key=len,
    )
    simulation_depth_required = len(longest_sequence) - 1

    for simulation_depth_type in ["simulation_depth_train", "simulation_depth_eval"]:
        if config[simulation_depth_type] is None:
            config[simulation_depth_type] = simulation_depth_required
        elif simulation_depth_required > config[simulation_depth_type]:
            raise ValueError(
                "Specified {} of {} is not sufficient to reach oracle minumum for {}, must be at least {}".format(
                    simulation_depth_type,
                    config[simulation_depth_type],
                    longest_sequence[0],
                    simulation_depth_required,
                )
            )


def check_save_config(config, train_exprs, eval_exprs):
    """Ensures that simulation_depth_train, simulation_depth_eval are sufficient to reach oracle min if known,
    and save to disk the config and train.pck."""
    if train_exprs and eval_exprs and config["oracle"]:
        assert all(e in eval_exprs for e in train_exprs)
        _ensure_oracle_reachable(config, eval_exprs)

    with utils.open_file_mkdir(
        os.path.join(config["result_save_path"], "config.json"), "w"
    ) as f:
        json.dump(config, f, sort_keys=True, indent=2)
    if train_exprs:
        with open(os.path.join(config["result_save_path"], "train.pck"), "wb") as f:
            pickle.dump(train_exprs, f)

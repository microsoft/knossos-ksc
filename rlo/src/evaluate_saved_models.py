import argparse
import json
import os
import sys
from typing import Iterable

from rlo import analytics
from rlo import config_utils
from rlo.compute_values import compute_values
from rlo.experiment_result import load_config
from rlo.expression_util import NamedExprWithEnv
from rlo import factory
from rlo.tf_model import ModelWrapper
from rlo import rewrites
from rlo.search_ops import AbstractSearcher
from rlo import utils

# Load model files previously saved from run_one_repetition and replay evaluation.
# This should produce identical results (for the same test exprs in the same sequence).


def replay_one_rep(config, log_prefix, eval_exprs):
    result_save_path = config["result_save_path"]
    with analytics.log_events_to_files(
        os.path.join(result_save_path, log_prefix)
    ), analytics.Scope(repetition=config["repetition"]), factory.regressor_from_config(
        config
    ) as model_wrapper:
        rng_for_gen_fn = factory.rng_factory_from_config(config)
        eval_rng = utils.rng(rng_for_gen_fn(0))

        searcher = factory.searcher_from_config(config, "eval")
        # Initial evaluation using untrained model and alpha 0.0 = uniform random search.
        # The StateValueRegressorModel has been initialized using the same config/seed so this should produce the same results
        # as the original (untrained) evaluation run, for both A* and Rollout.
        with analytics.Scope(generation=0):
            eval_over_expressions(
                searcher, model_wrapper, eval_rng, eval_exprs, untrained_model=True
            )

        saved_gens = utils.get_saved_model_files(
            factory.model_save_dir_from_config(config),
            # Support previously saved models with model_name different from "model"
            model_name=config.get("model_name", "model"),
        )
        for generation, path in saved_gens:
            # This repeats the seed-construction process of the Curriculum.
            eval_rng = utils.rng(rng_for_gen_fn(generation))
            with analytics.Scope(generation=generation):
                model_wrapper.load_weights(path)
                eval_over_expressions(searcher, model_wrapper, eval_rng, eval_exprs)


def eval_over_expressions(
    searcher: AbstractSearcher,
    model_wrapper: ModelWrapper,
    random_source,
    expressions: Iterable[NamedExprWithEnv],
    untrained_model: bool = False,
):
    for expr in expressions:
        searcher.eval(
            model_wrapper, random_source, expr, untrained_model=untrained_model
        )
    # Get value estimates for each expression at each time-left.
    # We'll log these out for every generation.
    compute_values(model_wrapper, expressions)


def main(cmd_args):
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="a run ID (e.g., 2019_01_06_13_15_48_13172) or path to a config.json file",
    )
    parser.add_argument("--eval_examples", type=str)
    parser.add_argument(
        "--rules",
        help="overwrite rules saved in config",
        choices=rewrites.available_rules(),
    )
    parser.add_argument(
        "--output",
        type=str,
        help="prefix for log file within experiment/repetition dir; defaults to eval_ (i.e. eval_events(_N).json)",
        default="eval_",
    )
    parser.add_argument(
        "--repetition", type=int, default=None, help="Run one repetition only"
    )
    parser.add_argument(
        "--num_parallel",
        type=int,
        default=1,
        help="Run evaluation in this number of parallel processes",
    )
    args = parser.parse_args(cmd_args)

    config = load_config(args.run_id)
    if args.repetition is not None:
        rep_config = config_utils.config_for_repetition(config, args.repetition)
    elif config["repetition"] is not None:
        rep_config = config
    else:
        config_utils.multiprocess(
            args.num_parallel,
            main,
            [
                (cmd_args + ["--repetition={}".format(i)])
                for i in range(config["num_repetitions"])
            ],
        )
        return
    # Evaluating one repetition.
    # Use <config> for paths to files, such as train.pck, that may only be present once.
    # Use <rep_config> for paths to outputs.
    if args.rules is not None:
        rep_config["rules"] = args.rules
    print(json.dumps(rep_config, indent=2))

    if args.eval_examples is None:
        _, eval_expr_set = factory.get_train_and_eval_exprs(config)
        eval_examples = eval_expr_set.named_exprenvs()
    else:
        eval_examples = utils.load_pickle(args.eval_examples)

    replay_one_rep(rep_config, args.output, eval_examples)


if __name__ == "__main__":
    main(sys.argv[1:])  # sys.argv[0] is this .py file

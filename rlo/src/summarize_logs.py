# mypy: ignore-errors
import os
import shutil

from rlo.experiment_result import load_config_events
from rlo.factory import get_train_and_eval_exprs
from rlo.flags import (
    check_save_config,
    make_config_for_scenario,
    make_parser,
    run_arguments,
)

from rlo.summarize_logs import summarize_logs
from rlo.utils import FunctionArgs as Args


def copy_files_for_plot(repetition_output_dirs, run_id, result_save_path):
    """ Given a list of directories, copy the contents to result_save_path.

    Note that we assume repetition_output_dirs is ordered and contains
    a directory with the name matching its position in the list. For example,
    if repetition_output_dirs = ["output_a", "output_b", "output_c"], we
    assume that there are directories
        output_a/Run_xxxx/0
        output_b/Run_xxxx/1
        output_c/Run_xxxx/2
    where xxxx is the run_id.

    Thus if result_save_path = "outputs/Run_xxxx", we will have
        outputs/Run_xxxx/0
        outputs/Run_xxxx/1
        outputs/Run_xxxx/2
    """
    for rep, original_output_dir in enumerate(repetition_output_dirs):
        original_save_path = os.path.abspath(
            "{}/Run_{}".format(original_output_dir, run_id)
        )
        # Find the repetition directory
        rep_dir = os.path.join(original_save_path, str(rep))
        if not os.path.isdir(rep_dir):
            raise ValueError(
                "Could not find a valid repetition directory in {}."
                " Maybe --num_repetitions is mismatching?".format(original_output_dir)
            )
        shutil.copytree(rep_dir, os.path.join(result_save_path, str(rep)))


def main():
    run_parser = make_parser(
        run_arguments
        + [
            Args("--loglevel", type=int, default=1, choices=[0, 1]),
            Args(
                "--repetition_output_dirs",
                type=str,
                nargs="*",
                help="list of directories containing the results of repetitions,"
                " if they are not located in config['result_save_path'].",
            ),
        ]
    )
    run_args, _ = run_parser.parse_known_args()
    if run_args.run_id is None:
        # (Redoing) plots for an existing config; scenario is that config
        config, _ = load_config_events(run_args.scenario)
    else:
        # Finishing a run
        # Allow unknown so that the additional command line arguments are ignored
        config = make_config_for_scenario(run_args.scenario, allow_unknown=True)

    train_set, eval_set = get_train_and_eval_exprs(config)

    if run_args.run_id is not None:
        # Finishing a run - save the config for the whole run.
        check_save_config(config, train_set.named_exprenvs(), eval_set.named_exprenvs())

    # if repetitions were run in separate (AML) runs, copy files to output_dir to load them
    if run_args.repetition_output_dirs:
        copy_files_for_plot(
            run_args.repetition_output_dirs,
            config["run_id"],
            config["result_save_path"],
        )
    summarize_logs(config, eval_set, loglevel=run_args.loglevel)


if __name__ == "__main__":
    main()

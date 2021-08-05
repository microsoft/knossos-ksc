# fmt: off
from typing import List
import os
import sys

from rlo import analytics
from rlo import config_utils
from rlo import factory
from rlo.flags import (make_parser, run_arguments, make_config_for_scenario, check_save_config)
from rlo.local_worker import LocalWorker
from rlo.memory_report import memory_report
from rlo.summarize_logs import summarize_logs


def run_one_repetition(config, train_expressions, eval_expressions):

    curriculum = factory.simul_search_curriculum_from_config(
        config, train_expressions, eval_expressions)

    # Alternatively, could omit the repetition field and infer from the logfile from which we load.
    result_save_path = config["result_save_path"]
    if not result_save_path.endswith(os.path.sep):
        result_save_path += os.path.sep
    with analytics.log_events_to_files(result_save_path), \
            analytics.Scope(repetition=config["repetition"]):
        analytics.event("start_repetition", device_id = os.environ.get("CUDA_VISIBLE_DEVICES", None))
        analytics.event("expression_summary", num_train_expr = len(train_expressions), num_test_expr = len(eval_expressions))

        # pylint: disable=no-member # __enter__ inherited from WorkerWithModel which has no schedule_*
        with LocalWorker(config) as worker:
            worker.schedule_work_requests_from(
                curriculum.request_initial(factory.seed_from_config(config)))
            worker.run()


def main(args: List[str]):
    run_parser = make_parser(run_arguments)
    run_args, _ = run_parser.parse_known_args(args)

    config = make_config_for_scenario(run_args.scenario, cmdline=args)
    train_set, eval_set = factory.get_train_and_eval_exprs(config)

    check_save_config(config, train_set.named_exprenvs(), eval_set.named_exprenvs())

    with memory_report():
        if config.get("repetition") is not None:
            run_one_repetition(config, train_set, eval_set)
            return # Return early - rest will be skipped by separate check

        config_utils.multiprocess(config["num_parallel"],
            main,
            [(args + [
                "--repetition={}".format(i),
                "--run_id={}".format(config["run_id"])
            ]) for i in range(config["num_repetitions"])])

        summarize_logs(config, eval_set)


if __name__ == "__main__":
    main(sys.argv[1:])  # sys.argv[0] is this .py file

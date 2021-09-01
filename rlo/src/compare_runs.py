import argparse

from rlo.compare_runs import compare_runs, compare_reps_within_run
from rlo.experiment_result import load_events_from_config, load_config_events


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "first",
        type=str,
        help="Run ID or path to config of first experiment (or only, to compare between reps)",
    )
    parser.add_argument(
        "--vs_eval_events",
        action="store_true",
        help="Compare one experiment events.json against eval_events.json from replay",
    )
    parser.add_argument(
        "second",
        type=str,
        nargs="?",
        help="Run ID/path of second experiment; required unless --vs_eval_events",
    )
    parser.add_argument(
        "--skip_event_types", nargs="+", default=[], help="event types to skip"
    )
    parser.add_argument(
        "--sort",
        action="store_true",
        help="Sort events (Ray dumps them out in nondeterministic order)",
    )
    args = parser.parse_args()

    def skip_event_types(e):
        if e["event"] not in args.skip_event_types:
            yield e

    first_config, first = load_config_events(args.first, event_filter=skip_event_types)
    if args.vs_eval_events:
        if args.second is not None:
            raise ValueError(
                "Comparing first experiment against its own eval_events, don't know what to do with second experiment"
            )
        second = load_events_from_config(
            first_config, prefix="eval_", event_filter=skip_event_types
        )
        # The eval_events don't include training.
        first = [e for e in first if "eval_expr" in e]
        second = [e for e in second if "eval_expr" in e]
        if any("gen" not in e for e in first):
            # First run was with merged datasets, so gen not present in logs.
            assert all("gen" not in e for e in first)
            second = [{k: v for k, v in e.items() if k != "gen"} for e in second]
    elif args.second is not None:
        _, second = load_config_events(args.second, event_filter=skip_event_types)
    elif first_config.get("seed_all_reps") is not None:
        compare_reps_within_run(first, sort=args.sort)
        return
    else:
        raise ValueError(
            "Require two experiments, or --vs_eval_events, or one experiment with config['seed_all_reps'] set"
        )
    compare_runs(first, second, sort=args.sort)


if __name__ == "__main__":
    main()

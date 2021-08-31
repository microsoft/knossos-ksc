import os
import json
import argparse

from rlo.experiment_result import load_config_events


def extract_dataset_from_events(
    events, generation=1, repetition=0, extract_with_fitted_values=None,
):
    events = (
        e
        for e in events
        if (
            e.get("repetition") == repetition
            and e.get("generation") == generation
            and e.get("event") == "distill_fit"
            and "target_and_fitted" in e
        )
    )

    return [
        (tl, e, targ, fit) if extract_with_fitted_values else (tl, e, targ)
        for event in events
        for e, seq in event["target_and_fitted"].items()
        for tl, (targ, fit) in enumerate(seq[1:], 1)
    ]


def main():
    parser = argparse.ArgumentParser(
        """
            Extract a dataset from an events_1.json file.
            Extracted dataset will be saved in the experiment folder as dataset_gen_N_rep_M.json

            To extract a dataset:
            $ python src/extract_dataset.py --run_id /path/to/experiment/with/events/files --generation 1 --repetition 0

            Normally the extracted dataset is in the form [(time_left, expr, target)...]. Flag --extract_with_fitted_values
            adds fitted values into the dataset, making it in the form [(time_left, expr, target, fitted)...]
        """
    )
    parser.add_argument(
        "--run_id",
        type=str,
        help="path to run_id containing events_1.json to generate dataset",
    )
    parser.add_argument(
        "--generation",
        type=int,
        default=1,
        help="generation to extract the dataset from",
    )
    parser.add_argument(
        "--repetition",
        type=int,
        default=0,
        help="repetition to extract the dataset from",
    )
    parser.add_argument(
        "--extract_with_fitted_values",
        action="store_true",
        help="whether to add fitted values into dataset",
    )
    args = parser.parse_args()

    config, events = load_config_events(args.run_id, verbosity=1)
    data_points = extract_dataset_from_events(
        events, args.generation, args.repetition, args.extract_with_fitted_values
    )
    dataset = {
        "run_id": config["run_id"],
        "gitlog": config["gitlog"],
        "data_points": data_points,
    }
    print("Total number of points in the dataset: ", len(data_points))

    with open(
        os.path.join(
            config["result_save_path"],
            f"dataset_gen_{args.generation}_rep_{args.repetition}.json",
        ),
        "w",
    ) as f:
        json.dump(dataset, f, indent=2)


if __name__ == "__main__":
    main()

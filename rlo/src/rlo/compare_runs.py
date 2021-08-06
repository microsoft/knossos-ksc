from typing import List, Dict

import numpy as np

from rlo import utils


def by_rep(es):
    return utils.group_by(
        [e for e in es if "repetition" in e], lambda e: int(e["repetition"])
    )


def filter_event_fields(events):
    return [
        {
            k: v
            for k, v in e.items()
            if k
            not in [
                "total_train_time",
                "total_train_cpu",
                "repetition",
                "device_id",
                "gnn_eval_time",
                "saved_state_file",
                "model_file",
            ]
        }
        for e in events
    ]


def sort_order(events):
    return sorted(
        events,
        key=lambda e: (
            e.get("generation", 0),
            e["event"],
            e.get("expr", e.get("eval_expr", "")),
            e.get("distill_net", ""),
        ),
    )


def compare_two_reps(rep1, rep2, desc):
    print(
        "Comparing repetition {} with {} events vs {}".format(
            desc, len(rep1), len(rep2)
        )
    )

    # Discard excess events, as one run could go for longer than the other
    for i, (e1, e2) in enumerate(
        zip(filter_event_fields(rep1), filter_event_fields(rep2))
    ):
        same_keys, diff_keys = same_and_different_keys(e1, e2)
        if diff_keys:
            print("same: " + str({k: e1[k] for k in same_keys}))
            print("first: " + str({k: e1[k] for k in diff_keys if k in e1}))
            print("second: " + str({k: e2[k] for k in diff_keys if k in e2}))
            raise ValueError(f"Repetitions {desc} differ at index {i}.")


def is_similar(v1, v2) -> bool:
    # Check if two (possibly nested) values are similar. Float values can differ by some tolerance; other data types must match exactly.
    # TODO #19704 instead of having this complicated logic and generous tolerance for float values, use torch.backends.cudnn.deterministic for determinism tests.
    if type(v1) != type(v2):
        print(f"{v1} ({type(v1)}) and {v2} ({type(v2)}) have different types.")
        return False
    if isinstance(v1, List):
        if len(v1) != len(v2):
            print(
                f"Lists have different lengths: first has {len(v1)} items, second has {len(v2)} items."
            )
            return False
        return all(is_similar(x1, x2) for x1, x2 in zip(v1, v2))
    elif isinstance(v1, Dict):
        _, diff_keys = same_and_different_keys(v1, v2)
        if diff_keys:
            return False
        return True
    elif isinstance(v1, float):
        # Floats don't have to match exactly
        return np.allclose(v1, v2, atol=1e-6)
    else:
        # Any other type does have to match exactly
        return v1 == v2


def same_and_different_keys(v1, v2):
    same_keys = set(
        k
        for k in frozenset(v1.keys()).intersection(v2.keys())
        if is_similar(v1[k], v2[k])
    )
    diff_keys = frozenset(v1.keys()).union(v2.keys()).difference(same_keys)
    return same_keys, diff_keys


def compare_runs(run1, run2, sort=False):
    sort_fn = sort_order if sort else lambda evs: evs

    def list_reps(evs):
        return frozenset([e["repetition"] for e in evs if "repetition" in e])

    if list_reps(run1) != list_reps(run2):
        raise ValueError("Runs have different sets of repetitions")
    for rep, (es1, es2) in sorted(utils.zip_values(by_rep(run1), by_rep(run2)).items()):
        compare_two_reps(sort_fn(es1), sort_fn(es2), desc=rep)


def compare_reps_within_run(events, sort=False):
    sort_fn = sort_order if sort else lambda evs: evs
    reps = sorted(by_rep(events).items())
    first_rep, first_events = reps[0]
    for rep, events in reps[1:]:
        compare_two_reps(
            sort_fn(first_events), sort_fn(events), "{} vs {}".format(first_rep, rep)
        )

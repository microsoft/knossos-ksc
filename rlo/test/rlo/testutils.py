# fmt: off
from typing import Optional, Mapping, Sequence
from collections import namedtuple
import json
import numpy as np
import os
import re

from rlo.expression import Expression
from rlo.expression_util import SymtabAndDefs, ExprWithEnv
from rlo import rewrites
from rlo import sparser
from ksc.type import Type
from rlo import utils

from rlo.pipelines import RawExample

def scenario_path(scenario_name):
    repo_root = os.path.join(os.path.dirname(__file__), os.pardir, os.pardir)
    return os.path.normpath(os.path.join(repo_root, "src", "scenarios", f"{scenario_name}.json"))

def parse_expr_typed(string_or_stream, var_types: Optional[Mapping[str, Type]]=None, default_var_type=Type.Float):
    e = sparser.parse_expr(string_or_stream)
    if var_types is None:
        var_types = {}
    return SymtabAndDefs(symtab={
        name: var_types.get(name, default_var_type) for name in e.free_var_names
    }).make_toplevel(e)

def get_config():
    return {
    "use_subtree_match_edges": True,
    "hidden_dim": 100,
    "output_hidden_dim": 80,
    "num_propagations": 5,
    "simulation_depth_train": 9,
    "simulation_depth_eval": 9,
    "seed_all_reps": 1,
    "num_gnn_blocks": 1,
    "stacked_gnn_double_hidden": False,
    "cost_normalization": "none",
    "loss": "pinball=0.9",
    "lr": 0.001,
    "decoder_readout": "sum",
    "graph_state_keep_prob": 0.5,
    "output_keep_prob": 0.4,
    "tensorflow": False,
    "aggregation_over_edge_types": "sum"
}

def make_rng(seed: Optional[int]=None):
    if seed is None:
        seed = np.random.randint(0, 2**31)
        print("Initializing from seed {}".format(seed))
    return utils.rng(seed)

def random_increasing(rng, max, num_vals):
    r = rng.uniform(0, max)
    return np.array([r * i/10 for i in range(0, num_vals)])

def assert_sequences_equal(actual, expected):
    print(f"actual={actual}")
    print(f"expected={expected}")
    assert len(actual) == len(expected)
    for act, exp in zip(actual, expected):
        if isinstance(exp, (list, tuple)):
            return assert_sequences_equal(act, exp)
        np.testing.assert_equal(act, exp)

RecordedEpisode = namedtuple("RecordedEpisode", ["lines", "costs", "actions"])

def parse_best_episodes_file(filename):
    with open(filename) as f:
        lines = [line.strip("\n") for line in f if line.startswith(";")]
    # split into sections
    sep = ";-----------------------------------------"
    sep_indices = [i for i, line in enumerate(lines) if line.startswith(sep)]
    assert (len(sep_indices) // 2) * 2 == len(sep_indices)
    sep_open, sep_close = zip(*[(sep_indices[i], sep_indices[i+1]) for i in range(0, len(sep_indices), 2)])
    section_names = [lines[i + 1].strip("; ") for i in sep_open]

    sections = {}
    for name, iclose, iopen in zip(section_names, sep_close, sep_open[1:] + (len(lines),)):
        local_lines = [lines[i] for i in range(iclose+1, iopen) if lines[i].startswith("; step")]
        sections[name] = ep = RecordedEpisode(
            lines=local_lines,
            costs=[
                float(utils.single_elem(m.groups()))
                for line in local_lines for m in [re.search(r"cost=([0-9e\.\+-]+)", line)]
                if m is not None
            ],
            actions=[
                (int(m.group(1)), rewrites.rule(m.group(2)))
                for line in local_lines for m in [re.search(r"action=\(([0-9]+), '([^']+)'\)", line)]
                if m is not None
            ]
        )
        assert len(ep.costs) == len(ep.lines), f"Expected {len(ep.lines)} costs, but got {len(ep.costs)} for lines {ep.lines}"
        assert len(ep.actions) == len(ep.lines) - 1, (
            f"Expected {len(ep.lines) - 1} actions, but got {len(ep.actions)}" # no action for the last line
        )
    return sections


def load_training_examples(
        dataset_file="datasets/value_dataset.json", shuffle=True) -> Sequence[RawExample]:
    with open(dataset_file) as f:
        dataset = json.load(f)
    dataset = dataset["data_points"]
    def mk_datapoint(t, expr_str, a):
        e = parse_expr_typed(expr_str)
        return t, e, a
    out = [mk_datapoint(*example) for example in dataset]
    return utils.permutation(make_rng(), out) if shuffle else out

def load_dataset(dataset_file: str, shuffle=True):
    with open(dataset_file) as f:
        dataset = json.load(f)['data_points']
    return np.random.permutation(dataset) if shuffle else dataset

def make_toplevel(e : Expression) -> ExprWithEnv:
    return SymtabAndDefs().make_toplevel(e)

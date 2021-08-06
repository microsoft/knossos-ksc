from collections import Counter
import random

import os
from rlo import pipelines
from rlo import rewrites
from rlo import sparser
from rlo import utils

rand = random.Random(0)
filename = os.path.join(
    os.path.dirname(os.path.realpath(__file__)), "ksc", "gmm", "gmm_test.kso"
)
print("Loading {}".format(filename))
name, test_expr = sparser.parse_defs(utils.read_file(filename))[-1]
print(f"{name} has {test_expr.expr.num_nodes} nodes")
rules = rewrites.get_rules("ml_rules_no_bind")


def main(rollouts, depth, subtree_match):
    pipe = pipelines.SparsePipeline(use_subtree_match_edges=subtree_match)
    edges = Counter()
    for _ in range(rollouts):
        e = test_expr
        for _step in range(depth):
            children = [rewrite.apply(e) for rewrite in rules.get_all_rewrites(e)]
            for ch in children:
                data = pipe.prepare_example(ch)
                edges[sum(edges_of_type.size for edges_of_type in data.edge_lists)] += 1
            e = rand.choice(children)
    total_edges = sum(num_edges * freq for num_edges, freq in edges.items())
    print(f"Edges: average {total_edges/sum(edges.values())}, dist {edges}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--num_rollouts", type=int, default=1)
    parser.add_argument("--depth", type=int, default=25)
    parser.add_argument(
        "--no_use_subtree_match_edges",
        action="store_false",
        dest="use_subtree_match_edges",
    )
    args = parser.parse_args()

    main(args.num_rollouts, args.depth, args.use_subtree_match_edges)

import functools
import pytest
from typing import Tuple, Callable

from rlo.analytics import LogEventsToList
from rlo.astar_search import AStarSearcher, Expander, LoggingExpander
from rlo.beam_search import PseudoBeamSearcher
from rlo.factory import maxing_algs
from rlo import expr_sets
from rlo.hybrid_search import HybridSearcher, MergeHandling
from mock_random_net import RandomStateValueModel, RandomPolicyModel
from rlo.node_evaluation_cache import NodeEvaluationCache
from rlo import rewrites
from rlo.rollouts import (
    ValueBasedRolloutSearcher,
    PolicyBasedRolloutSearcher,
    SearchController,
    StopOnSuccessController,
)
from rlo.search_ops import best_episode_to_log
from rlo.search_tree import (
    max_over_episodes,
    max_with_accumulator,
    NodeAction,
    output_dot,
    SearchTreeNode,
    StateValueSearchTreeNode,
    TransitionsMixin,
    SetChildMixin,
    Transition,
)
from rlo import utils
from testutils import make_rng, parse_expr_typed


def dummy_maxing(_results):
    assert False


def get_test_expr():
    return parse_expr_typed("(let a (div 1.0 x) (div a (add 1.0 a)))")


def test_best_episode_to_log():
    def mk(e, tl, rule_name):
        node = StateValueSearchTreeNode(parse_expr_typed(e), tl)
        return NodeAction(
            node,
            action=None
            if rule_name is None
            else utils.single_elem(
                list(rewrites.rule(rule_name).get_all_rewrites(node.exprenv))
            ),
        )

    eps = [
        [
            mk("(mul x (div y y))", 10, "commute_mul"),
            mk("(mul (div y y) x)", 9, "div_by_self"),
            mk("(mul 1.0 x)", 8, "mul_one"),
            mk("x", 6, "exp_add_zero"),  # Rest will be truncated:
            mk("(add 0.0 x)", 5, "add_zero"),
            mk("x", 4, None),
        ],
        [
            mk("(mul x (div y y))", 10, "commute_mul"),
            mk("(mul (div y y) x)", 8, "commute_mul"),
            mk("(mul x (div y y))", 7, "commute_mul"),
            mk("(mul (div y y) x)", 6, "commute_mul"),
            mk("(mul x (div y y))", 5, None),
        ],
        [
            mk("(mul x (div y y))", 10, "div_by_self"),
            mk("(mul x 1.0)", 9, "commute_mul"),
            mk("(mul 1.0 x)", 8, "commute_mul"),
            mk("(mul x 1.0)", 7, "commute_mul"),
            mk("(mul 1.0 x)", 6, "mul_one"),
            mk("x", 6, None),
        ],
    ]
    actual = best_episode_to_log(eps)
    assert actual.init_expr == "(mul x (div y y))"
    assert actual.rewrite_seq_costs == [
        ((0, "commute_mul"), 4.0),
        ((1, "div_by_self"), 4.0),
        ((0, "mul_one"), 2.0),
        ((None, None), 0.0),
    ]


def check_rollout_tree(rollouts, rules):
    root_id = id(rollouts[0][0].node)
    assert all(id(rollout[0].node) == root_id for rollout in rollouts)
    for rollout in rollouts:
        for i, step in enumerate(rollout):
            node = step.node
            assert hasattr(node, "_evaluation") or i == 0
            assert (node.probs is not None) or (i == len(rollout) - 1)
            if node.is_expanded():
                expr = node.exprenv
                # print("Checking expression: {}".format(expr))
                expected_children = [
                    rewrite.apply(expr) for rewrite in rules.get_all_rewrites(expr)
                ]
                assert set(expected_children) == set([c.exprenv for c in node.children])
                assert all(c.time_left == node.time_left - 1 for c in node.children)


# Clear _best_costs between checks
def clear_max(rollouts):
    all_nodes = set([s.node for episode in rollouts for s in episode])
    for n in all_nodes:
        n._prev_best_costs = n._best_costs
        n._best_costs = None


def count_maxing_same_better(rollouts):
    all_nodes = set([s.node for episode in rollouts for s in episode])
    nodes_by_expr = utils.group_by(all_nodes, lambda n: n.exprenv).values()
    max_over_episodes(rollouts)
    check_maxing_consistent(rollouts)
    clear_max(rollouts)

    # ...now recompute _best_costs using more thorough algorithm
    max_with_accumulator(rollouts)
    check_maxing_consistent(rollouts, check_over_tree=True, check_bounds_tight=True)

    # Now check max_with_accumulator is always at least as good
    better = 0
    same = 0
    for nodes in nodes_by_expr:
        longest = max(nodes, key=lambda n: n.time_left)
        for tl in range(1, longest.time_left + 1):
            now, prev = (longest._best_costs[tl], longest._prev_best_costs[tl])
            assert now <= prev
            if now < prev:
                better += 1
            else:
                same += 1
    print(
        "max_with_accumulator same as max_over_episodes in {} cases, better in {}".format(
            same, better
        )
    )

    return (same, better)


def check_maxing_consistent(episodes, check_over_tree=False, check_bounds_tight=False):
    # assumes already maxed
    if check_over_tree:
        # checks all nodes reachable from the root node including nodes that are not in
        # episodes (not compatible with rollout with max_over_episodes)
        all_nodes = episodes[0][0].node.all_nodes
    else:
        # checks only nodes in episodes, appropriate for rollout with max_over_episodes
        assert not check_bounds_tight  # We do not expect tight bounds in that setup.
        all_nodes = set([s.node for episode in episodes for s in episode])

    for expr, nodes in utils.group_by(all_nodes, lambda n: n.exprenv).items():
        # Maxing should have put the same CumMinSequence at every node with this expr
        node = nodes[0]
        assert all(
            n._best_costs.critical_values == node._best_costs.critical_values
            for n in nodes[1:]
        )

        # CumMinSequence guarantees monotonicity, but check within bounds
        assert all(cost >= 0 and cost <= expr.cost() for cost in node._best_costs)
        for tl, cost in enumerate(node._best_costs):
            # Compute the lowest cost (bound) that could be found by maxing.
            # Include children only if reachable in number of steps.
            children = [] if tl == 0 else [ch for n in nodes for ch in n.children]
            locally_consistent_cost = min(
                [expr.cost()] + [ch.min_cost_in_steps(tl - 1) for ch in children]
            )
            # If this constraint is satisfied everywhere (globally), we can be sure that
            # every best_cost computed by maxing is achievable by rewrites:
            assert cost >= locally_consistent_cost
            if check_bounds_tight:
                # Also check we have the lowest solution satisfying that constraint
                # (as max_with_accumulator computes via iteration to fixpoint).
                assert cost == locally_consistent_cost


def test_rollouts_simplify_rules(seed=None, expected_same_better_counts=None):
    rng = make_rng(seed)
    rules = rewrites.get_rules("simplify_rules")
    searcher = ValueBasedRolloutSearcher(
        rules=rules,
        maxing=dummy_maxing,
        simulation_depth=10,
        num_episode_clusters=3,
        alpha_test=0.0,
        num_positive_examples=0,
        max_num_episodes=0,
    )
    e = parse_expr_typed("(div (div 1.0 x) (add 1.0 (div 1.0 x)))")
    rollouts, _cache = searcher._search(
        RandomStateValueModel(rng, num_time_heads=11),
        rng,
        e,
        SearchController(128),
        target_cost=None,
        alpha=0.5,
    )
    check_rollout_tree(rollouts, rules)
    same_better_counts = count_maxing_same_better(rollouts)
    if expected_same_better_counts is not None:
        assert same_better_counts == expected_same_better_counts


def test_rollouts_binding_rules(seed=None, expected_same_better_counts=None):
    # Give the Searcher a poisoned maxing algorithm to make sure it's not doing any maxing itself.
    rng = make_rng(seed)
    rules = rewrites.get_rules("binding_rules")
    searcher = ValueBasedRolloutSearcher(
        rules=rules,
        maxing=dummy_maxing,
        simulation_depth=10,
        num_episode_clusters=3,
        alpha_test=0.0,
        num_positive_examples=0,
        max_num_episodes=0,
    )
    e = get_test_expr()
    rollouts, _cache = searcher._search(
        RandomStateValueModel(rng, num_time_heads=11),
        rng,
        e,
        SearchController(128),
        target_cost=None,
        alpha=0.5,
    )
    check_rollout_tree(rollouts, rules)
    same_better_counts = count_maxing_same_better(rollouts)
    if expected_same_better_counts is not None:
        assert same_better_counts == expected_same_better_counts


def test_policy_net_rollouts(seed=None):
    rng = make_rng(seed)
    rules = rewrites.get_rules("simplify_rules")
    searcher = PolicyBasedRolloutSearcher(
        rules=rules,
        maxing=max_over_episodes,
        simulation_depth=10,
        num_episode_clusters=3,
        alpha_test=0.0,
        num_positive_examples=0,
        max_num_episodes=0,
    )
    e = get_test_expr()
    rollouts, cache = searcher._search(
        RandomPolicyModel(rng, len(list(rules)), num_time_heads=11),
        rng,
        e,
        SearchController(128),
        alpha=1.0,
    )
    count_maxing_same_better(rollouts)
    output_dot("output.dot", rollouts)  # Use result of last maxing (with accumulator)
    start_node = rollouts[0][0].node
    min_cost_seen = cache.best_cost_seen
    min_by_maxing = start_node.min_cost_in_steps(start_node.time_left)
    assert min_cost_seen == min_by_maxing

    # build dataset
    dataset = searcher._build_dataset(utils.seed(rng), rollouts)
    print(len(dataset.get_examples()))


def test_policy_net_training_search(seed=None):
    rng = make_rng(seed)
    rules = rewrites.get_rules("simplify_rules")
    searcher = PolicyBasedRolloutSearcher(
        rules=rules,
        maxing=max_over_episodes,
        simulation_depth=10,
        num_episode_clusters=3,
        alpha_test=0.0,
        num_positive_examples=0,
        max_num_episodes=10,
    )
    e = get_test_expr()
    searcher.training_search(
        RandomPolicyModel(rng, len(list(rules)), num_time_heads=11), rng, e, e.cost()
    )


@pytest.mark.parametrize("cls", [AStarSearcher, HybridSearcher, PseudoBeamSearcher])
def test_astarlike(
    cls, expander_factory=Expander, seed=None, expected_same_better_counts=None
):
    # Give the Searcher a poisoned maxing algorithm to make sure it's not doing any maxing itself.
    rng = make_rng(seed)
    searcher = cls(
        num_episode_clusters=3,
        rules=rewrites.get_rules("simplify_rules"),
        maxing=dummy_maxing,
        simulation_depth=10,
        max_gnn=1000,
        expander_factory=expander_factory,
    )
    e = parse_expr_typed("(div (div 1.0 x) (add 1.0 (div 1.0 x)))")
    episodes, _cache = searcher._search(
        RandomStateValueModel(rng, num_time_heads=11), rng, e
    )
    same_better_counts = count_maxing_same_better(episodes)
    if expected_same_better_counts is not None:
        assert same_better_counts == expected_same_better_counts


@pytest.mark.parametrize("maxing", maxing_algs.values())
def test_astar_maxing_consistent(maxing, seed=0):
    rng = make_rng(seed)
    searcher = AStarSearcher(
        num_episode_clusters=3,
        rules=rewrites.get_rules("binding_rules"),
        maxing=dummy_maxing,
        simulation_depth=10,
        max_gnn=1000,
    )
    e = get_test_expr()
    episodes, _cache = searcher._search(
        RandomStateValueModel(rng, num_time_heads=11), None, e
    )
    maxing(episodes)
    check_maxing_consistent(episodes, check_over_tree=True)


@pytest.mark.parametrize("maxing", maxing_algs.values())
def test_astar_training_search(maxing, seed=0):
    num_clusters = 100
    searcher = AStarSearcher(
        num_episode_clusters=num_clusters,
        rules=rewrites.get_rules("simplify_rules"),
        maxing=maxing,
        simulation_depth=10,
        max_gnn=30,
    )
    e = get_test_expr()
    episodes, _cache = searcher._search(
        RandomStateValueModel(make_rng(seed), num_time_heads=11), None, e
    )
    assert len(episodes) < num_clusters
    # Repeat search with identical RandomStateValueModel.
    # Clustering will fail unless at least num_clusters episodes are passed to it, so AStarSearcher will have to copy.
    rng = make_rng(seed)
    searcher.training_search(
        RandomStateValueModel(rng, num_time_heads=11), rng, e, target_cost=5
    )


def test_hybrid_search(seed=0):
    e = get_test_expr()
    # Convert episodes from list[list[SearchTreeNode]] into a comparable, hashable type.
    def comparable_episodes(searcher, model_wrapper, rng) -> Tuple[Tuple[str]]:
        episodes, _cache = searcher._search(model_wrapper, rng, e)
        return tuple(  # type: ignore
            tuple("{}@{}".format(n.node.exprenv.expr, n.node.time_left) for n in ep)
            for ep in episodes
        )

    # Check that varying the merge_handling and alpha value changes the results
    def get_episodes(merge_handling, alpha, check_search_seed_matters):
        searcher = HybridSearcher(
            rules=rewrites.get_rules("simplify_rules"),
            simulation_depth=10,
            merge_handling=merge_handling,
            alpha=alpha,
            max_gnn=100,
            maxing=None,
        )
        rng = make_rng(seed)
        episodes = comparable_episodes(
            searcher, RandomStateValueModel(rng, num_time_heads=11), rng
        )
        # Check determinism
        rng = make_rng(seed)
        episodes2 = comparable_episodes(
            searcher, RandomStateValueModel(rng, num_time_heads=11), rng
        )
        assert episodes == episodes2
        # Now repeat with different search seed but same RandomStateValueModel
        rng = make_rng(seed)
        model_wrapper = RandomStateValueModel(rng, num_time_heads=11)
        _ = rng.integers(0, 0x7FFFFFFF, dtype=int)  # discard a value, so now different
        episodes3 = comparable_episodes(searcher, model_wrapper, rng)
        assert check_search_seed_matters == (episodes != episodes3)
        return episodes

    all_episodes = [
        get_episodes(merge_handling, alpha, True)
        for merge_handling in list(MergeHandling)
        for alpha in [0.2, 0.5]
    ]
    assert len(set(all_episodes)) == len(all_episodes)
    # At high enough alpha, STOP and CONTINUE become equivalent: every node is deterministic;
    # any time we CONTINUE through a merge node, we repeat the previous path.
    # Infinite alpha with CONTINUE is ruled out by the constructor, so take something high enough
    # to have the same effect in practice (i.e. make the softmax policy deterministic)
    stop = get_episodes(MergeHandling.STOP, float("inf"), False)
    assert get_episodes(MergeHandling.CONTINUE, 1000000, False) == stop
    assert get_episodes(MergeHandling.AVOID, float("inf"), False) != stop

    with pytest.raises(ValueError, match="CONTINUE is equivalent to STOP"):
        HybridSearcher(
            rules=rewrites.get_rules("simplify_rules"),
            max_gnn=100,
            simulation_depth=10,
            merge_handling=MergeHandling.CONTINUE,
            alpha=float("inf"),
            maxing=None,
        )


def test_more_maxing_strictly_better():
    # These counts will change if either (a) ValueBasedRolloutSearcher changes,
    # OR (b) the rulesets change: the order of the rules, or new rules being added/removed
    # that match on the expressions above.
    # As a test of maxing algorithms, this would be better using a fixed/hand-constructed search tree.
    # In its present form, it is also a repeatable test that Searchers behave as expected;
    # if rule ordering changes are frequent enough, then we should rewrite this test to use its own fixed ruleset.
    test_rollouts_simplify_rules(1, (98, 49))
    test_rollouts_binding_rules(0, (340, 293))


@pytest.mark.notquick
def test_rollouts_blas_combined():
    """ This was a corner case where accumulator maxing beat the previous subtree maxing,
        for reasons not entirely understood. It may not test anything useful now, although
        may cover more cases than other tests due to its scale. The counts may be fragile. """
    seed = 0
    rules = rewrites.get_rules("ml_rules_no_bind")
    searcher = ValueBasedRolloutSearcher(
        rules=rules,
        maxing=dummy_maxing,
        simulation_depth=49,
        alpha_test=5.0,
        num_positive_examples=10,
        max_num_episodes=20,
    )
    expr_set = expr_sets.get_expression_set("ksc/blas/blas_combined.kso")
    rollouts, _cache = searcher._search(
        RandomStateValueModel(utils.rng(seed), num_time_heads=50),
        seed,
        expr_set.named_exprenvs()[-1].exprenv,
        search_ctrl=StopOnSuccessController(
            searcher._max_num_episodes,
            searcher._num_positive_examples,
            target_cost=None,
        ),
        target_cost=None,
        alpha=0.0,
    )
    check_rollout_tree(rollouts, rules)
    expected_same_better_counts = (2896, 18505)
    actual_same_better_counts = count_maxing_same_better(rollouts)
    assert actual_same_better_counts == expected_same_better_counts


@pytest.mark.parametrize("expander_factory", [Expander, LoggingExpander])
def test_astar_like_more_maxing_strictly_better(expander_factory):
    test_astarlike(AStarSearcher, expander_factory, 0, (165, 94))
    test_astarlike(PseudoBeamSearcher, expander_factory, 0, (168, 91))


def test_trivial_maxing_does_not_crash():
    count_maxing_same_better(
        [[NodeAction(StateValueSearchTreeNode(parse_expr_typed("x"), 10), None)]]
    )


@pytest.mark.parametrize("max_gnn", [30, 50, 60, 122])
def test_astar_logging_expander(max_gnn, batch_size=10, seed=0):
    searcher = AStarSearcher(
        rules=rewrites.get_rules("simplify_rules"),
        simulation_depth=10,
        max_gnn=max_gnn,
        batch_size=batch_size,
        expander_factory=LoggingExpander,
        maxing=None,
    )
    e = get_test_expr()
    with LogEventsToList(verbosity_limit=1) as logger:
        searcher._search(
            RandomStateValueModel(make_rng(seed), num_time_heads=11), None, e
        )
    rollout_event = utils.single_elem(
        [e for e in logger.log_items if e["event"] == "rollout_end"]
    )
    expansions_event = utils.single_elem(
        [e for e in logger.log_items if e["event"] == "rollout_end_verbose"]
    )
    # the difference is the number of leaf nodes expanded
    assert rollout_event["expanded"] <= len(expansions_event["expansions"])
    assert all(
        [
            num_evals <= max_gnn + batch_size
            for num_evals, time_left, cost in expansions_event["expansions"]
        ]
    )


def _search_exhaustively(root_node, cache, expander):
    need_work = set([root_node])
    seen = set()
    while len(need_work) > 0:
        node = need_work.pop()
        if node in seen:
            continue
        seen.add(node)
        expander(node, cache)
        for next_node in node.children:
            need_work.add(next_node)
    assert cache.eval_queue_length == len(seen)
    cache.process_batches()
    assert cache.eval_queue_length == 0


# Timeless search.


def expand_by_transitions(node, cache, rules):
    child_expr_actions = [
        (rewrite, rewrite.apply(node.exprenv))
        for rewrite in rules.get_all_rewrites(node.exprenv)
    ]
    node.transitions = [Transition(a, cache.get_node(c)) for a, c in child_expr_actions]
    assert node.is_expanded()


def expand_by_set_child_treelike(node, cache, rules):
    # Do not create edges for rewrites to nodes already found.
    node.actions = list(rules.get_all_rewrites(node.exprenv))
    for rewrite in node.actions:
        child_expr = rewrite.apply(node.exprenv)
        if not cache.check_existing_node(child_expr):
            node.set_child(rewrite, cache.get_node(child_expr))


def timeless_search_helper(
    expr, expand_func: Callable, search_tree_node_class, seed=None
):
    rng = make_rng(seed)
    expr = parse_expr_typed(expr)
    rules = rewrites.get_rules("simplify_rules")

    cache = NodeEvaluationCache(
        RandomStateValueModel(rng, num_time_heads=11), 1, search_tree_node_class,  # type: ignore
    )
    root_node = cache.get_node(expr)
    _search_exhaustively(root_node, cache, functools.partial(expand_func, rules=rules))
    # Both max_with_accumulator and check_maxing_consistent expect
    # to be passed episodes, but only use them to extract the root node.
    fake_episodes = [[NodeAction(root_node, None)]]
    max_with_accumulator(fake_episodes)
    check_maxing_consistent(
        fake_episodes, check_over_tree=True, check_bounds_tight=True
    )
    return root_node


@pytest.mark.parametrize("expr", ["(mul a (add x y))", "(mul a (div 1.0 a))"])
def test_timeless_treelike(expr):
    # Set children only for actions that do not create cycles or merges
    class SetChildSearchTreeNode(SetChildMixin, SearchTreeNode):
        pass

    root_node = timeless_search_helper(
        expr, expand_by_set_child_treelike, SetChildSearchTreeNode
    )

    for node in root_node.all_nodes:
        assert node.has_evaluation()
        best_cost = node.min_cost_in_steps(float("inf"))
        # Since some edges are missing, some expressions may not be optimizable
        print(f"{node}: best cost={best_cost}")
        for child in node.children:
            child_best = child.min_cost_in_steps(float("inf"))
            print(f"  - child={child}: best cost={child_best}")
            assert best_cost <= child_best


@pytest.mark.parametrize("expr", ["(mul a (add x y))", "(mul a (div 1.0 a))"])
def test_timeless_cyclic(expr):
    # Set all children, creating merges and cycles. These expressions
    # have no "dead ends" so all intermediates are eventually optimized.
    class TransitionSearchTreeNode(TransitionsMixin, SearchTreeNode):
        pass

    root_node = timeless_search_helper(
        expr, expand_by_transitions, TransitionSearchTreeNode
    )

    optimized_cost = root_node.min_cost_in_steps(float("inf"))
    for node in root_node.all_nodes:
        assert node.has_evaluation()
        # Every intermediate has a route to the best node.
        assert node.min_cost_in_steps(float("inf")) == optimized_cost

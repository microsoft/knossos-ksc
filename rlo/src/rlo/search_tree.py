# mypy: ignore-errors
from collections import OrderedDict
from typing import (
    Dict,
    Generic,
    Iterator,
    List,
    NamedTuple,
    Optional,
    Protocol,
    Tuple,
    TypeVar,
    Union,
)

import numpy as np

from rlo import utils
from rlo.accumulators import UpperBoundsAccumulator
from rlo.cum_sequence import CumMinSequence
from rlo.expression_util import ExprWithEnv
from rlo.rewrites import Rewrite


class SearchTreeNode:
    """
    An object used to represent a search tree node that keeps track of following quantities relevant for doing search:
    - Expression at this node
    - The possible children that one could get to by applying rewrites
        (self._children_by_actions)
    - Probabilities for transitioning to each child (self._probs). This can be set and used by the searcher algorithms as desired
    - An evaluation of the value function at this node (self._evaluation).
    """

    @staticmethod
    def to_cache_key(expr: ExprWithEnv):
        return expr

    def __init__(self: "NodeType", exprenv: ExprWithEnv):
        self._exprenv = exprenv
        self._children_by_actions: Optional[Dict[Rewrite, Optional["NodeType"]]] = None
        # (Note that we tend to treat "Rewrite" and "action" somewhat interchangeably, but strictly speaking,
        #    a Rewrite applies to an Expression, whereas
        #    an "action" in RL terminology applies to "states" i.e. nodes and includes decrementing time-left.)
        # We rely on the only Rewrites looked up within a node being the same Rewrite objects as those stored in it.
        self._evaluation = None
        self._probs = None
        # self._best_costs will be filled in by max_over_episodes / max_with_accumulator
        self._best_costs: Optional[CumMinSequence] = None

    @property
    def exprenv(self) -> ExprWithEnv:
        return self._exprenv

    # TODO: Fill in type
    @property
    def probs(self):
        return self._probs

    @probs.setter
    def probs(self, probs):
        if self._probs is not None:
            raise AttributeError("probs may not be set more than once")
        self._probs = probs

    def min_cost_in_steps(self, steps):
        if self._best_costs is None:
            # Rollout with max_over_episodes only maxes nodes in the episodes i.e. that are expanded.
            assert not self.is_expanded()
            return self.exprenv.cost()
        return self._best_costs[steps]

    @property
    def all_nodes(self):
        res = set()

        def traverse(n):
            if n in res:
                return  # Optimize traversal of DAGs with merges
            res.add(n)
            for c in n.children:
                traverse(c)

        traverse(self)
        return res

    def is_expanded(self) -> bool:
        """Returns whether this node has ever had its transitions or actions set.
        children can be empty even when is_expanded() is True, if there were
        no rewrites or none of the actions have yet been performed.
        """
        return self._children_by_actions is not None

    @property
    def children(self) -> List["NodeType"]:
        """Returns a list of non-None children; this will be empty if is_expanded() is False
        """
        if not self.is_expanded():
            return []
        return list(c for c in self._children_by_actions.values() if c is not None)

    def set_evaluation(self, evaluation) -> None:
        """
        This method is called by NodeEvaluationCache to set the model evaluation
        (value or policy log probabilities) cache on the node. It is just a cache of the
        model value for the expression (e.g. possibly an array for multiple time-left).
        """
        assert not self.has_evaluation()
        assert evaluation is not None
        self._evaluation = evaluation

    def has_evaluation(self):
        return self._evaluation is not None

    def evaluation(self):
        return self._evaluation


NodeType = TypeVar("NodeType", bound=SearchTreeNode)


class NodeAction(NamedTuple, Generic[NodeType]):
    node: NodeType
    action: Union[None, Rewrite]


class Transition(NamedTuple):
    action: Optional[Rewrite]
    next_node: NodeType


class PolicyValueEvaluation(NamedTuple):
    value: float
    log_probs: np.ndarray


Episode = List[NodeAction]


class TransitionList(Generic[NodeType]):
    """ A class representing a sequence of Transitions """

    def __init__(
        self,
        initial_node: Optional[NodeType] = None,
        steps: Optional[List[Transition]] = None,
    ):
        # The first Transition should have `action=None`, `next_node=initial_node`
        self._steps = []
        if steps is not None:
            self.extend(steps)
        else:
            self.append(Transition(None, initial_node))
        # TODO: we might want to prevent self._steps = [Transition(None, None)]
        # either initial_node or steps should be specified

    def append(self, transition: Transition):
        assert isinstance(transition, Transition)
        self._steps.append(transition)

    def extend(self, steps: List[Transition]):
        for step in steps:
            self.append(step)

    def to_episode(self) -> Episode:
        actions, nodes = zip(*self._steps)
        # note: actions starts with None
        return [NodeAction(n, a) for n, a in zip(nodes, actions[1:] + (None,))]

    def __add__(self, steps):
        o = TransitionList(steps=self._steps)
        o.extend(steps)
        return o


class TransitionsMixin(SearchTreeNode):
    _children_by_actions: Optional[Dict[Rewrite, Optional[SearchTreeNode]]]

    @property
    def transitions(self) -> Iterator[Transition]:
        """ This will produce no elements if is_expanded() is False """
        actions_children = self._children_by_actions if self.is_expanded() else {}
        return (Transition(a, c) for a, c in actions_children.items())

    @transitions.setter
    def transitions(self, transitions: Iterator[Transition]):
        """ Sets (action, next_node) pairs ensuring that next_node only appears once
            even if multiple actions lead to the same next node.
        """
        if self._children_by_actions is not None:
            raise AttributeError("transitions may not be set more than once")
        assert all(isinstance(t, Transition) for t in transitions)
        # Uniquify by next_node to make sure that the same child is not registered multiple times
        self._children_by_actions = OrderedDict(
            (a, c) for a, c in utils.uniquify(transitions, key=lambda x: x.next_node)
        )


class SetChildMixin(SearchTreeNode):
    _children_by_actions: Optional[Dict[Rewrite, Optional[SearchTreeNode]]]

    @property
    def actions(self) -> List[Rewrite]:
        """ Returns a list of actions that can be taken at the current node
        """
        if self._children_by_actions is None:
            return None
        return list(self._children_by_actions.keys())

    @actions.setter
    def actions(self, actions: Iterator[Rewrite]) -> None:
        if self._children_by_actions is not None:
            raise AttributeError("actions may not be set more than once")
        assert all(isinstance(a, Rewrite) for a in actions)
        self._children_by_actions = OrderedDict((a, None) for a in actions)

    def set_child(self, action: Rewrite, node):
        assert (
            self._children_by_actions is not None
            and action in self._children_by_actions
        )
        if self._children_by_actions[action] is None:
            # It is possible that the same action is chosen multiple times
            # in different rollouts
            self._children_by_actions[action] = node


class TimeLeftSearchTreeNode(SearchTreeNode):
    """
    Base class for StateValueSearchTreeNode and ActionSearchTreeNode.
    Represents an intermediate stage/step in optimization, i.e. expression and time left.
    """

    @staticmethod
    def to_cache_key(expr: ExprWithEnv, time_left: int) -> Tuple[ExprWithEnv, int]:
        return (expr, time_left)

    def __init__(self, expr: ExprWithEnv, time_left: int):
        super().__init__(expr)
        self._time_left = time_left

    @property
    def time_left(self) -> int:
        return self._time_left

    def evaluation(self, cost_per_step: Optional[float] = None):
        time_left = min(self._time_left, len(self._evaluation) - 1)
        if cost_per_step is None:
            return self._evaluation[time_left]
        value_with_cost_per_step = self._evaluation[: time_left + 1] - (
            np.arange(0, time_left + 1) * cost_per_step
        )
        # Ignore the model value for time_left 0 and insert a zero. This ensures the max will be >=0.
        value_with_cost_per_step[0] = 0
        return value_with_cost_per_step.max()


class StateValueSearchTreeNode(TimeLeftSearchTreeNode, TransitionsMixin):
    """A SearchTreeNode all of whose child-nodes are set at once, and where no child is repeated. Lookup by Rewrite is thus not necessary.
    """

    def __init__(self, exprenv: ExprWithEnv, time_left: int):
        super().__init__(exprenv, time_left)

    def __repr__(self):
        return "StateValueSearchTreeNode(expr={}, time_left={})".format(
            self.exprenv.expr, self.time_left
        )


class ActionSearchTreeNode(TimeLeftSearchTreeNode, SetChildMixin):
    """ A SearchTreeNode which
        (1) does not require that all children are present;
        (2) may have more than one action that leads to the same child.
    """

    def __init__(self, expr, time_left):
        super().__init__(expr, time_left)

    def __repr__(self):
        return "ActionSearchTreeNode(expr={}, time_left={})".format(
            self.exprenv.expr, self.time_left
        )

    def evaluation(self):
        return PolicyValueEvaluation(
            self._evaluation.values[self.time_left],
            self._evaluation.log_probs[:, :, self.time_left],
        )


class MaxingCallable(Protocol):
    """A protocol (for structural typing) for maxing functions defining their call signature.

    Maxing functions should update SearchTreeNodes in the passed episodes in-place.
    """

    def __call__(
        self, episodes: List[Episode], max_depth: float = float("inf")
    ) -> None:
        pass


def max_over_episodes(episodes: List[Episode], max_depth=float("inf")):
    """
    Fills in all the SearchTreeNode's _best_costs to reflect the best cost seen within that many steps of an actual episode.
    Example: we may extract B -> C from an episode A -> B -> C -> D.
    Where one episode goes along nodes A -> B -> C -> D, and another goes A -> E -> C -> F, paths from E to D and B to F will *not* be used.
    """
    for episode in episodes:
        for index, step in enumerate(episode):
            node = step.node
            if node._best_costs is None:
                node._best_costs = CumMinSequence.from_critical_values(
                    [(0, node.exprenv.cost())]
                )
            # Apply min from within this episode. CumMinSequence ignores any update that doesn't strengthen bounds.
            for steps in range(index + 1, len(episode)):
                if steps - index > max_depth:
                    break
                node._best_costs.update(
                    steps - index, episode[steps].node.exprenv.cost()
                )
    # Now find the best cost across SearchTreeNodes for the same Expression but different time_left
    all_nodes = set([s.node for episode in episodes for s in episode])
    nodes_by_expression = utils.group_by(all_nodes, lambda node: node.exprenv)
    for nodes in nodes_by_expression.values():
        assert all(nodes[0].exprenv == n.exprenv for n in nodes[1:])
        # Merge all the 'CumMinSequence's together into one (which one is arbitrary)
        canonical_best_costs = nodes[0]._best_costs
        for n in nodes[1:]:
            canonical_best_costs.update_from_seq(n._best_costs)
        for n in nodes[1:]:
            n._best_costs.update_from_seq(canonical_best_costs)


def accumulate_upper_bounds(
    root_node: SearchTreeNode, max_depth=float("inf")
) -> Dict[ExprWithEnv, CumMinSequence]:
    """Compute upper bounds on costs for all nodes in `root_node.all_nodes`."""
    # UpperBoundsAccumulators do lots of dictionary look-ups, so we assign each
    # expression an index and accumulate upper bounds on those indices.
    exprenvs = []
    indices = {}

    def get_index(exprenv) -> int:
        index = indices.get(exprenv)
        if index is None:
            index = len(exprenvs)
            exprenvs.append(exprenv)
            indices[exprenv] = index
        return index

    acc = UpperBoundsAccumulator(lambda i: exprenvs[i].cost(), max_depth)
    for node in root_node.all_nodes:
        src = get_index(node.exprenv)
        for child in node.children:
            dst = get_index(child.exprenv)
            acc.add_edge(src, dst)

    return {exprenvs[i]: v for i, v in acc.upper_bounds.items()}


def max_with_accumulator(episodes: List[Episode], max_depth=float("inf")) -> None:
    """
    Fill in all `SearchTreeNode`s' _best_costs with an `UpperBoundsAccumulator`.

    This finds most strict upper bounds believed to be possible.
    """

    root_node = episodes[0][0].node
    upper_bounds = accumulate_upper_bounds(root_node, max_depth)

    for node in root_node.all_nodes:
        bounds = upper_bounds.get(node.exprenv)
        if bounds is None:
            bounds = CumMinSequence.from_critical_values([(0, node.exprenv.cost())])
        node._best_costs = bounds


def output_dot(output_file: str, rollouts):
    """ Given a list of rollouts, outputs the search tree as a dot file. """
    import graphviz

    root = rollouts[0][0].node

    def get_name(node):
        return "{}.{}".format(str(node.exprenv.expr), node.time_left)

    graph = graphviz.Digraph()
    all_nodes = set()
    needs_visit = [root]
    while len(needs_visit) > 0:
        node = needs_visit.pop()
        if node not in all_nodes:
            name = get_name(node)
            min_cost_str = (
                ""
                if node._best_costs is None
                else f", min_cost={node.min_cost_in_steps(node.time_left)}"
            )
            label = f"<{node.exprenv.expr} ({node.time_left})<br/>cost={node.exprenv.cost()}{min_cost_str}>"
            graph.node(
                name, label, shape="triangle" if len(node.children) == 0 else "oval"
            )
            all_nodes.add(node)
            for child in node.children:
                child_name = get_name(child)
                graph.edge(name, child_name)
                needs_visit.append(child)
    with open(output_file, "w") as f:
        f.write(graph.source)

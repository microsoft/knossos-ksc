import pytest

from rlo import utils
from rlo.search_tree import (
    max_with_accumulator,
    NodeAction,
    StateValueSearchTreeNode,
    Transition,
)
from rlo.expression import Expression, EF
from testutils import make_toplevel
from rlo.rewrites import Rewrite
from ksc.type import Type


class DummyRewrite(Rewrite):
    def __init__(self):
        super().__init__(0, self)

    @staticmethod
    def _apply_to_subtree(exp):
        raise AssertionError()


def test_max_with_accumulator_stack_overflow():
    def mk_node(time_left):
        nonlocal cost
        expr = make_toplevel(
            EF.Stop(Expression.Variable(f"v{cost}", type=Type.Float), cost=cost)
        )
        res = StateValueSearchTreeNode(expr, time_left=time_left)
        cost -= 1
        return res

    # Make WIDTH episodes starting from a common root with time-left=DEPTH.
    # The last expression of each episode will be the same as the first expression,
    # among the children of root, of the next episode:
    #          ROOT
    #    /   / .... \   \
    #  ea.1 eb.1   ey.1  ez.1
    #    .   . .... .     .
    #  ea.9 eb.9   ey.9  ez.9
    #  eb.1 ec.1   ez.1
    # The longest episode is thus DEPTH steps, but the longest chain of rewrites in the graph of Expressions
    # is thus ea.1 ---> ea.9 -> eb.1 ---> eb.9 -> ec.1 ---> ec.9 ----> ey.1 --> ey.9 -> ez.1 --> ez.9
    # i.e. ~~ WIDTH * DEPTH steps. (I think WIDTH * (DEPTH - 1).)
    WIDTH = 26
    DEPTH = 10
    cost = WIDTH * DEPTH * 2
    root = mk_node(DEPTH)

    def make_chains():
        prev = None
        for _ in range(WIDTH):
            for j in range(DEPTH - 1, 0, -1):
                node = mk_node(j)
                if j == DEPTH - 1:
                    # Start of an episode. Root will link to this.
                    yield node
                    if prev is not None:
                        # Link last *Expression* in previous episode to this Expression (but with different time_left)
                        prev.transitions = [
                            Transition(
                                DummyRewrite(),
                                StateValueSearchTreeNode(node.exprenv, time_left=0),
                            )
                        ]
                else:
                    prev.transitions = [Transition(DummyRewrite(), node)]
                prev = node

    root.transitions = [Transition(DummyRewrite(), n) for n in make_chains()]
    episodes = [[NodeAction(root, None)]]  # All we actually need for maxing
    # The recursion limit here is *not* intended to be the exact point at which the problem occurs
    # (that will depend on the length of the cycle in the max_with_accumulator call graph, among other details).
    with utils.override_recursion_limit(WIDTH * DEPTH // 2):
        # If this doesn't throw an exception, it's not necessarily a problem per se,
        # rather the issue which (the max-depth-limit argument to maxing algorithms) is intended to deal with, has gone.
        # (Note also, the crash depends upon the order with which max_with_acculumator processes the edges:
        # if ea.1 -> ea.2 is processed first, and ez.8 -> ez.9 last, the final edge causes costs to propagate all the way to the beginning, which overflows;
        # if ez.8 -> ez.9 were processed first, giving good cost bounds in ez.8; then edge ez.7->ez.8 were processed, propagating good costs to ez.7;
        #     then ez.6 -> ez.7 were processed, propagating good costs to ez.6 -- then each edge only propagates cost updates along one step, so no overflow.
        # Hence the test is likely to be rather dependent on the ordering in which the edges are seen.)
        with pytest.raises(RecursionError):
            max_with_accumulator(episodes)  # no limit, so roughly WIDTH*DEPTH

        # We'll be able to compute with any max_depth<=some fraction of WIDTH*DEPTH, possibly WIDTH*DEPTH//2;
        # this is not intended to be an exact limit, but "low enough" and representative of our normal usage.
        max_with_accumulator(episodes, max_depth=DEPTH)


if __name__ == "__main__":
    test_max_with_accumulator_stack_overflow()

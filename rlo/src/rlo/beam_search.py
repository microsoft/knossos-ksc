from rlo.astar_search import AStarSearcher, EarliestEnqueuingCache
from rlo.utils import UrgencyQueue
from rlo.tf_model import ModelWrapper


class PseudoBeamSearcher(AStarSearcher):
    """ A variant of Beam Search.
        Strictly, Beam Search would first expand the start node;
        then choose the best N nodes among the start nodes children,
        then the best N among their children (=grandchildren), then the best N among....
        and so on, for some "beam width" N.

        This variant instead explicitly controls the total number of GNN evaluations performed
        (which depends on both N and the branching factor of the nodes being expanded after merging+pruning),
        as well as incorporating the A* behaviour of only expanding an Expression at the greatest time_left we've seen it.

        To do this, it explores down columns or depth-first, rather than breadth-first (dealing with a whole row at a time) as Beam Search does.
        That is, we expand the best node at depth 1, then the best at depth 2, ... simulation_depth;
        then go back to the top and expand the best node (among remaining candidates) at depth 1, then 2, and so on.
        This should explore nodes with the same distribution of depths as Beam Search would,
        but there is a slight difference in the selection, as the following example demonstrates.

        Consider startnode S with children A (priority 1) and B (priority 2);
            let A have children A3 (priority 3) and A4 (priority 4),
            and B with children B1 (priority 1) and B2 (priority 2).
        (Note the inversion of priorities - A's children have higher priority than B's children,
         even tho B has higher priority than A.)

        Beam Search with width 2 would expand S, then A and B, then A3 and A4.

        Pseudo-Beam Search expands S, then B, then B2 (as it's the best available candidate; A3 and A4 have not been found yet),
            and then loops round, expanding A, then A4.
        That is, PBS is forced to choose B2 rather than A3 because only B2 has been discovered the first time at depth==2.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def _search_to_tree(
        self,
        model_wrapper: ModelWrapper,
        _random_source,
        expander,
        start_expr,
        **_kwargs
    ):
        open_lists = [UrgencyQueue() for _ in range(self._simulation_depth + 1)]
        cache = EarliestEnqueuingCache(
            model_wrapper,
            self._batch_size,
            lambda node: open_lists[node.time_left].put(self.urgency(node), node),
        )

        # Note this will cause the root Expression to be evaluated, which isn't strictly necessary (if there are no routes back to it).
        # But it'll help detect/handle routes back to the start node better if such are found.
        start_posn = cache.get_node(start_expr, self._simulation_depth)

        while cache.total_posns_evaluated < self._max_gnn:
            acted = False
            for next_tl in range(self._simulation_depth, 0, -1):
                if cache.has_sufficient_posns_to_eval() or (
                    open_lists[next_tl].empty() and cache.eval_queue_length > 0
                ):
                    cache.process_batches()
                    if cache.total_posns_evaluated >= self._max_gnn:
                        break
                # Expand a node. Loop to find one that isn't pruned.
                # (Note that if there were nodes at this depth, but all of them are pruned,
                # we won't have called process_batches above - a slight bug.)
                while not open_lists[next_tl].empty():  # Loop to find a non-pruned node
                    node = open_lists[next_tl].get()
                    # If the node has smaller time left than the best in the closed list, then ignore the node as it is a duplicate.
                    if node._pruned:
                        continue
                    acted = True
                    expander(node, cache)
                    break  # from inner while loop
            if not acted:
                break  # Nothing left we can do
        return start_posn, cache, {"unexplored": sum([len(o) for o in open_lists])}

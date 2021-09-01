from typing import Generic, TypeVar, Dict, Set, Any, Tuple, Union, Iterable
from collections import OrderedDict, defaultdict
from graphviz import Digraph, Graph

Dot = Union[Graph, Digraph]

N = TypeVar("N")  # Node


class GraphVisualizer(Generic[N]):
    def __init__(self):
        self._nodes: Dict[N, Dict[str, Any]] = OrderedDict()
        self._clusters: Dict[Any, Set[N]] = defaultdict(lambda: set())
        self._edges: Dict[Tuple[N, N], Dict[str, Any]] = OrderedDict()

    @property
    def nodes(self) -> Iterable[N]:
        return self._nodes.keys()

    def edges(self) -> Iterable[Tuple[N, N]]:
        return self._edges.keys()

    def update_node(self, node: N, cluster: Any = None, **attrs):
        attrs = {k: v for k, v in attrs.items() if v is not None}
        if node in self._nodes:
            self._nodes[node].update(attrs)
        else:
            self._nodes[node] = attrs
        if cluster is not None:
            self._clusters[cluster].add(node)

    def update_edge(self, src: N, dst: N, **attrs):
        key = (src, dst)
        if key in self._edges:
            self._edges[key].update(attrs)
        else:
            self._edges[key] = attrs

    def add_to(self, dot: Dot) -> None:
        remaining = OrderedDict(self._nodes)
        for cluster_key, cluster_nodes in self._clusters.items():
            for node in cluster_nodes:
                with dot.subgraph(name=f"cluster_{cluster_key}") as c:
                    # c.attr(style="filled", color="lightgrey")
                    if node in remaining:
                        c.node(str(node), **remaining.pop(node))
                    else:
                        raise ValueError(f"node {node} appeared in multiple clusters")

        for node, attrs in remaining.items():
            dot.node(str(node), **self._nodes[node])

        for (src, dst), attrs in self._edges.items():
            dot.edge(str(src), str(dst), **attrs)
        return dot

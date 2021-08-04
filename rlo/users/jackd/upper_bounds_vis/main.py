import abc
import os
from collections import OrderedDict
from typing import Tuple

from graphviz import Digraph

from rlo.accumulators import UpperBoundsAccumulator

from visualizer import GraphVisualizer


def update_augmented(augmented: GraphVisualizer, bounds):
    for n, seq in bounds.items():
        for i, v in seq.critical_values():
            augmented.update_node((n, i), label=str(v))


def dfs(start, neighbors):
    stack = [start]
    visited = set()
    while stack:
        top = stack.pop()
        if top not in visited:
            yield top
            stack.extend(neighbors[top])
            visited.add(top)


class Renderer:
    def __init__(self, *visualizers: GraphVisualizer):
        self._visualizers = visualizers
        self._frame = 0

    @abc.abstractmethod
    def _render(self, dot: Digraph, frame: int):
        raise NotImplementedError("Abstract method")

    def add_frame(self):
        dot = Digraph()
        for vis in self._visualizers:
            vis.add_to(dot)
        self._render(dot, self._frame)
        self._frame += 1

    def __enter__(self):
        return self

    def __exit__(self, *args, **kwargs):
        pass


def save_gif(filenames, path, duration=1):
    import imageio

    images = []
    for filename in filenames:
        images.append(imageio.imread(filename))
    imageio.mimsave(path, images, duration=duration)


class ShowBlock(Renderer):
    def _render(self, dot: Digraph):
        dot.render(view=True)
        input()


class PngWriter(Renderer):
    def __init__(
        self, *visualizers: GraphVisualizer, make_gif: bool = True, folder: str = "./",
    ):
        self._folder = folder
        self._make_gif = make_gif
        super().__init__(*visualizers)

    def _path(self, frame: int):
        return os.path.join(self._folder, f"frame{frame:02d}")

    def _render(self, dot: Digraph, frame: int):
        dot.render(format="png", filename=self._path(frame))

    def __exit__(self, *args, **kwargs):
        if not self._make_gif:
            return

        filenames = [f"{self._path(frame)}.png" for frame in range(self._frame)]
        save_gif(filenames, os.path.join(self._folder, "upper-bounds-accumulation.gif"))


def main():
    # flip this if you want it to look like maxing - it's just a hack on labels
    max_labels = False

    values = {"a": 3, "b": 2, "c": 1, "d": 5, "e": 0, "f": 9}

    edges = OrderedDict(
        {
            "a": ("b",),
            "b": ("a", "c", "f"),
            "c": ("b", "d"),
            "d": ("c", "e"),
            "e": ("d", "f"),
            "f": ("e",),
        }
    )
    colors = OrderedDict(
        {
            "a": "#FFBE33",
            "b": "#3CFF33",
            "c": "#337AFF",
            "d": "#E333FF",
            "e": "#FF3361",
            "f": "#F5EC42",
        }
    )

    max_value = max(values.values())

    def label(value):
        if max_labels:
            value = max_value - value
        return str(value)

    # build rewrite graph visualizer
    rewrite: GraphVisualizer[str] = GraphVisualizer()

    for node, color in colors.items():
        rewrite.update_node(
            node, style="filled", fillcolor=color, label=label(values[node])
        )

    # add rewrite edges - will be emboldened as explored
    for src, dsts in edges.items():
        for dst in dsts:
            rewrite.update_edge(src, dst, color="grey")

    # create augmented graph
    max_depth = 5
    augmented: GraphVisualizer[Tuple[str, int]] = GraphVisualizer()
    for n, color in colors.items():
        for i in range(max_depth + 1):
            augmented.update_node(
                (n, i), fillcolor=f"{color}22", style="filled", label="", cluster=n
            )
        dsts = edges[n]
        for i in range(max_depth):
            augmented.update_edge((n, i + 1), (n, i), color="grey")
            for dst in dsts:
                augmented.update_edge((n, i + 1), (dst, i), color="grey")

    start = "a"
    augmented.update_node(
        (start, 0), fillcolor=colors[start], label=label(values[start])
    )

    acc = UpperBoundsAccumulator(values.get)
    ub = acc.upper_bounds

    def update_values():
        """Update values (labels) in augmented visualizer."""
        for node, bounds in ub.items():
            color = colors[node]
            for i, v in enumerate(bounds.to_list(max_depth + 1)):
                augmented.update_node((node, i), label=label(v), fillcolor=f"{color}22")
            for i in bounds.critical_values.keys():
                augmented.update_node((node, i), fillcolor=color)

    with PngWriter(rewrite, augmented, folder="output") as frame_writer:
        frame_writer.add_frame()
        for src in dfs(start, edges):
            for dst in edges[src]:
                acc.add_edge(src, dst)
                # make rewrite edge bold
                rewrite.update_edge(src, dst, color="black")
                # make augmented edges darker
                for i in range(max_depth):
                    augmented.update_edge((src, i + 1), (dst, i), color="black")
                # update augmented labels
                update_values()
                frame_writer.add_frame()


if __name__ == "__main__":
    main()

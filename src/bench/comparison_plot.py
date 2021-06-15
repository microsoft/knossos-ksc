from dataclasses import dataclass
import pytest_benchmark
import pytest_benchmark.storage
import pytest_benchmark.storage.file
import pytest_benchmark.utils
import dateutil.parser
import datetime
from matplotlib.figure import Figure
import matplotlib.pyplot as plt
from collections import defaultdict
from shutil import copyfile


storage = pytest_benchmark.utils.load_storage(".benchmarks", logger=None, netrc=None)


@dataclass(frozen=True)
class FigureBundle:
    figure: Figure
    axis: plt.axes


@dataclass(frozen=True)
class FigureLookup:
    method: str
    configuration: str


groupedbenchmarks = defaultdict(list)
for path, content in storage.load():

    time = dateutil.parser.isoparse(content["commit_info"]["time"])
    for benchmark in content["benchmarks"]:

        # Before this date, everything was sqrl
        # TODO: migrate the store
        # TODO: generalise to more than sqrl
        if time.date() < datetime.date(2021, 6, 1) or "sqrl" in benchmark["name"]:
            testname = benchmark["name"].split("[")[0]
            groupedbenchmarks[testname].append((time, benchmark))
        else:
            print("Found non-sqrl benchmark, tools need extending")
            print(benchmark["name"])


def labelkey(name):
    if "Knossos" in name:
        return "rx"
    if "PyTorch Nice" in name:
        return "yo"
    if "PyTorch" in name:
        return "bo"
    return "go"


def make_figure():
    figure = plt.figure()
    axis = figure.add_subplot(111)

    axis.tick_params(axis="x", rotation=50)
    axis.set_xlabel("Commit time")
    axis.set_ylabel("median (microseconds)")
    return FigureBundle(figure=figure, axis=axis)


figures = defaultdict(make_figure)

for test in ("test_forward", "test_backwards", "test_inference"):
    for time, benchmark in groupedbenchmarks[test]:
        group_name = benchmark["group"]

        axis = figures[FigureLookup(method=test, configuration=group_name)].axis
        method = benchmark["name"].split("-")[1]  # TODO: harden, use extra_info?

        # TODO: generalise to more than sqrl
        axis.set_title(f"sqrl {test} {group_name}")
        axis.plot(
            time, (benchmark["stats"]["median"] * 1000), labelkey(method), label=method
        )

for figure_lookup, figure_bundle in figures.items():
    handles, labels = axis.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    axis.legend(by_label.values(), by_label.keys())

    filename = f"build/sqrl_{figure_lookup.method}_{figure_lookup.configuration}.svg".replace(
        " ", "_"
    )
    print(f"saving {filename}")

    figure_bundle.figure.savefig(filename, bbox_inches="tight")

copyfile("src/bench/sqrl.html", "build/sqrl.html")

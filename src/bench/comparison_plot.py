from dataclasses import dataclass
import pytest_benchmark
import pytest_benchmark.storage
import pytest_benchmark.storage.file
import pytest_benchmark.utils
import dateutil.parser
import datetime
from matplotlib.figure import Figure
from matplotlib.axes import Axes
import matplotlib.pyplot as plt
from collections import defaultdict
from shutil import copyfile


storage = pytest_benchmark.utils.load_storage(".benchmarks", logger=None, netrc=None)


@dataclass(frozen=True)
class FigureBundle:
    figure: Figure
    axes: Axes


@dataclass(frozen=True)
class FigureLookup:
    method: str
    configuration: str


groupedbenchmarks = defaultdict(list)
for path, content in storage.load():

    time = dateutil.parser.isoparse(content["commit_info"]["time"])
    for benchmark in content["benchmarks"]:

        # Before this date, everything was sqrl
        # TODO: migrate the store so we don't need to do data checks
        # TODO: generalise to more than sqrl
        if time.date() < datetime.date(2021, 6, 1) or "sqrl" in benchmark["name"]:
            testname = benchmark["name"].split("[")[0]  # TODO: harden, use extra_info?
            groupedbenchmarks[testname].append((time, benchmark))
        else:
            print(
                "Found non-sqrl benchmark, tools need extending https://msrcambridge.visualstudio.com/Knossos/_workitems/edit/19590"
            )
            print(benchmark["name"])


labelkey = defaultdict(lambda: "go")
labelkey["Knossos"] = "rx"
labelkey["PyTorch Nice"] = "yo"
labelkey["PyTorch"] = "bo"


def make_figure():
    figure = plt.figure()
    axes = figure.add_subplot(111)

    axes.tick_params(axis="x", rotation=50)
    axes.set_xlabel("Commit time")
    axes.set_ylabel("median (milliseconds)")
    return FigureBundle(figure=figure, axes=axes)


figures = defaultdict(make_figure)

for test in ("test_forward", "test_backwards", "test_inference"):
    for time, benchmark in groupedbenchmarks[test]:
        group_name = benchmark["group"]

        axes = figures[FigureLookup(method=test, configuration=group_name)].axes
        method = benchmark["name"].split("-")[1]  # TODO: harden, use extra_info?

        # TODO: generalise to more than sqrl
        axes.set_title(f"sqrl {test} {group_name}")
        axes.plot(
            time, (benchmark["stats"]["median"] * 1000), labelkey[method], label=method
        )

for figure_lookup, figure_bundle in figures.items():
    handles, labels = figure_bundle.axes.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    figure_bundle.axes.legend(by_label.values(), by_label.keys())

    figure_bundle.axes.set_ylim(bottom=0.0)

    filename = f"build/sqrl_{figure_lookup.method}_{figure_lookup.configuration}.svg".replace(
        " ", "_"
    )
    # print(f"saving {filename}")

    figure_bundle.figure.savefig(filename, bbox_inches="tight")

copyfile("src/bench/sqrl.html", "build/sqrl.html")

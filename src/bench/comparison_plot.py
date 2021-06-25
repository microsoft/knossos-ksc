from dataclasses import dataclass
from typing import Dict
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
    test_name: str
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


for benchmark_name in ["sqrl"]:

    figures: Dict[FigureLookup, FigureBundle] = defaultdict(make_figure)
    data_count = defaultdict(
        lambda: defaultdict(lambda: defaultdict(int))
    )  # test_name / configuration / method

    for test_name in ("test_forward", "test_backwards", "test_inference"):
        for time, benchmark in groupedbenchmarks[test_name]:
            configuration = benchmark["group"]

            axes = figures[
                FigureLookup(test_name=test_name, configuration=configuration)
            ].axes
            method = benchmark["name"].split("-")[1]  # TODO: harden, use extra_info?

            data_count[test_name][configuration][
                method
            ] += 1  # Count items to mark missing ones

            axes.set_title(f"{benchmark_name} {test_name} {configuration}")
            axes.plot(
                time,
                (benchmark["stats"]["median"] * 1000),
                labelkey[method],
                label=method,
                fillstyle="none",
            )

    for figure_lookup, figure_bundle in figures.items():

        methods_count = data_count[figure_lookup.test_name][figure_lookup.configuration]
        most_values = max(
            data_count[figure_lookup.test_name][figure_lookup.configuration].values()
        )

        handles, labels = figure_bundle.axes.get_legend_handles_labels()
        by_label = dict(zip(labels, handles))

        labels_with_incomplate_marking = [
            label if methods_count[label] == most_values else label + "(incomplate)"
            for label in by_label.keys()
        ]

        figure_bundle.axes.legend(by_label.values(), labels_with_incomplate_marking)

        figure_bundle.axes.set_ylim(bottom=0.0)

        filename = f"build/{benchmark_name}_{figure_lookup.test_name}_{figure_lookup.configuration}.svg".replace(
            " ", "_"
        )
        # print(f"saving {filename}")

        figure_bundle.figure.savefig(filename, bbox_inches="tight")

    # TODO: read in template, and parameterise with benchmark_name
    copyfile("src/bench/sqrl.html", f"build/{benchmark_name}.html")

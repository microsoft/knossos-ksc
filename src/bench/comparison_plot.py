from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Any
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


storage = pytest_benchmark.utils.load_storage(".benchmarks", logger=None, netrc=None)


@dataclass(frozen=True)
class FigureBundle:
    figure: Figure
    axes: Axes


@dataclass(frozen=True)
class FigureLookup:
    test_name: str
    configuration: str


def make_template(benchmark_name, testandconfigurations):
    plotsstring = ""

    for test_name, configurations in testandconfigurations.items():
        # A reverse string ASCIIbetical sort may not be entirely desirable, consider parsing out the ints
        # and sorting by them in order. This is OK for existing sqrl and vgelu examples, small to big.
        for configuration in sorted(configurations, reverse=True):
            plotsstring += f"""<img src="{benchmark_name}_{test_name}_{configuration.replace(" ", "_")}.svg">\n"""
        plotsstring += """<br/>"""

    # Consider using a serious HTML templating library
    return f"""<html>
    <head></head>
    <style>
        img {{
            width: 33%;
        }}
    </style>
    <body>
        {plotsstring}
    </body>
</html>"""


groupedbenchmarks: Dict[str, Dict[str, Any]] = defaultdict(lambda: defaultdict(list))
for path, content in storage.load():

    time = dateutil.parser.isoparse(content["commit_info"]["time"])
    for benchmark in content["benchmarks"]:
        benchmarkfullname = benchmark["name"]
        # TODO: migrate the store so we don't need to do data checks
        # TODO: start use extra_info to store benchmark and test name
        # Before this date, everything was sqrl
        benchmarkshortname = (
            benchmarkfullname.split("[")[1].split("_")[0]
            if time.date() > datetime.date(2021, 6, 1)
            else "sqrl"
        )
        testname = benchmarkfullname.split("[")[0]  # TODO: harden, use extra_info?
        groupedbenchmarks[benchmarkshortname][testname].append((time, benchmark))


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


dest = Path("build/benchmarks")
dest.mkdir(parents=True, exist_ok=True)

for benchmark_name, benchmark_value in groupedbenchmarks.items():

    figures: Dict[FigureLookup, FigureBundle] = defaultdict(make_figure)
    data_count = defaultdict(
        lambda: defaultdict(lambda: defaultdict(int))
    )  # test_name / configuration / method

    configurations = defaultdict(set)

    for test_name in ("test_forward", "test_backwards", "test_inference"):
        for time, benchmark in benchmark_value[test_name]:
            configuration = benchmark["group"]

            axes = figures[
                FigureLookup(test_name=test_name, configuration=configuration)
            ].axes
            method = benchmark["name"].split("-")[1]  # TODO: harden, use extra_info?

            data_count[test_name][configuration][
                method
            ] += 1  # Count items to mark missing ones

            configurations[test_name].add(configuration)

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

        labels_with_incomplete_marking = [
            label if methods_count[label] == most_values else label + "(incomplete)"
            for label in by_label.keys()
        ]

        figure_bundle.axes.legend(by_label.values(), labels_with_incomplete_marking)

        figure_bundle.axes.set_ylim(bottom=0.0)

        filename = (
            dest
            / f"{benchmark_name}_{figure_lookup.test_name}_{figure_lookup.configuration}.svg".replace(
                " ", "_"
            )
        )

        figure_bundle.figure.savefig(filename, bbox_inches="tight")

    htmlreport = make_template(benchmark_name, configurations)

    with open(dest / f"{benchmark_name}.html", "w") as file:
        file.write(htmlreport)

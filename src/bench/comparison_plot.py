import os
import pytest_benchmark
import pytest_benchmark.storage
import pytest_benchmark.storage.file
import pytest_benchmark.utils
import dateutil.parser
import datetime
import matplotlib.pyplot as plt
from collections import defaultdict

storage = pytest_benchmark.utils.load_storage(".benchmarks", logger=None, netrc=None)

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
    fig = plt.figure()
    axis = fig.add_subplot(111)

    axis.tick_params(axis="x", rotation=50)
    axis.set_xlabel("Commit time")
    axis.set_ylabel("median (seconds)")
    return (fig, axis)


figures = defaultdict(make_figure)

for test in ("test_forward", "test_backwards", "test_inference"):
    for time, benchmark in groupedbenchmarks[test]:
        group_name = benchmark["group"]

        _, axis = figures[(test, group_name)]
        method = benchmark["name"].split("-")[1]  # TODO: harden, use extra_info?

        axis.set_title(f"sqrl {test} {group_name}")
        axis.plot(time, benchmark["stats"]["median"], labelkey(method), label=method)

for (test, group_name), (figure, axis) in figures.items():
    handles, labels = axis.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    axis.legend(by_label.values(), by_label.keys())

    figure.savefig(
        f"build/sqrl_{test}_{group_name}.svg".replace(" ", "_"), bbox_inches="tight"
    )

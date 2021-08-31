import argparse
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

from rlo import experiment_result
from rlo import plotting
from rlo import utils


def task_type(e):
    return e["taskname"].split("_")[0]


worker_event_types = ["run_start", "worker_joined", "worker_died", "run_finished"]


def plot_ray_tasks(events, outfile, title_suffix):
    idle_time = sum(float(e["secs"]) for e in events if e["event"] == "workers_idle")
    task_events = [e for e in events if e["event"] == "ray_recv_result"]
    worker_events = [e for e in events if e["event"] in worker_event_types]
    deaths = [e["time_lost"] for e in worker_events if e["event"] == "worker_died"]
    num_timeouts = len(
        [
            e
            for e in worker_events
            if e["event"] == "worker_died" and e["reason"] is None
        ]
    )
    events_by_type = utils.group_by(task_events, task_type)
    event_types = sorted(events_by_type.keys())
    task_colors = {k: plotting.colors(i) for i, k in enumerate(event_types, 3)}
    fig, axs = plt.subplots(2 if len(worker_events) == 0 else 3, figsize=(12, 12))
    # Plot 1. Pie chart: transmission overhead + idle time + weights-loading, summed over all event types;
    # plus fraction in each event type
    data = [
        (
            "Loading weights",
            sum(e["weight_load_time"] for e in task_events),
            "lightgrey",
        ),
        ("Transmission", sum(e["transmit_time"] for e in task_events), "black"),
        ("Worker idle", idle_time, "darkgrey"),
        (f"Deaths inc ({num_timeouts}) timeouts", sum(deaths), "pink"),
    ] + [
        (
            f"{k} ({len(events_by_type[k])})",
            sum(e["wrkr_task_time"] for e in events_by_type[k]),
            task_colors[k],
        )
        for k in event_types
    ]
    total_time = sum(t for _name, t, _col in data)
    start = 0
    for label, width, color in data:
        axs[0].barh(label, width, left=start, color=color)
        num = "{} ({}%)".format(round(width), round(100 * width / total_time))
        right = start + width
        # Print 'num' in the largest gap, next to the bar
        on_left = start > total_time - right
        axs[0].text(
            start if on_left else right,
            label,
            num,
            ha="right" if on_left else "left",
            va="center",
        )
        start = right
    axs[0].set_title("Utilization of {} worker-seconds".format(round(total_time)))
    axs[0].set_xlabel("Total time on all workers")

    # Plot 2: event durations + transmission overhead on each task
    bucket_len = int(np.ceil(len(task_events) / 80))
    axs[1].set_title(
        f"Times for {len(task_events)} tasks"
        + (f" ({bucket_len}-task averages)" if bucket_len > 1 else "")
    )
    overhead_ax = axs[1].twinx()
    axs[1].set_yscale("log")
    axs[1].set_ylabel("transmit_time---total_task_time")
    overheads = []
    for start_idx in range(0, len(task_events), bucket_len):
        bucket = np.array(
            [
                (
                    task_type(e),
                    e["transmit_time"],
                    e["weight_load_time"] + e["wrkr_task_time"],
                )
                for e in task_events[start_idx : start_idx + bucket_len]
            ],
            dtype=object,
        )
        # Add overhead to remote time, making local time
        bucket[:, 2] += bucket[:, 1]
        by_task = utils.group_by(bucket, lambda task_row: task_row[0]).items()
        for i, (task_typ, tasks_of_type) in enumerate(by_task):
            tasks_of_type = np.array(tasks_of_type)[:, 1:]
            mean_times = tasks_of_type.mean(axis=0)
            mins = tasks_of_type.min(axis=0)
            maxs = tasks_of_type.max(axis=0)
            axs[1].errorbar(
                # Two separate bars: total time above transmission time
                [start_idx + i * bucket_len / len(by_task)] * 2,
                mean_times,
                fmt="o",
                yerr=[
                    mean_times - mins,  # low extent
                    maxs - mean_times,  # high extent
                ],
                color=task_colors[task_typ],
            )
        bucket_sum = bucket[:, 1:].sum(axis=0)
        overheads.append(bucket_sum[0] / bucket_sum[1])
    axs[1].set_xlabel("Tasks in completion order")
    # On RHS axis of same subplot, average of transmission overhead
    overhead_ax.set_ylabel("transmission overhead/1")
    overhead_ax.set_ylim(0, 1)

    overhead_ax.plot(range(0, len(task_events), bucket_len), overheads, color="black")

    # Plot 3: workers joined, died, max workers, unusable
    workers_joined = len([e for e in worker_events if e["event"] == "worker_joined"])
    axs[2].set_title("Workers ({} died, {} joined)".format(len(deaths), workers_joined))
    worker_events.sort(key=lambda e: e["time"])
    axs[2].plot(
        [e["time"] for e in worker_events],
        [e["num_workers"] for e in worker_events],
        label="available",
    )
    axs[2].plot(
        [e["time"] for e in worker_events],
        [e["unusable_workers"] for e in worker_events],
        label="unusable",
    )
    axs[2].set_ylabel("#workers")
    axs[2].set_xlabel("Wall time")
    axs[2].legend()

    fig.tight_layout(rect=[0, 0, 1, 0.975], h_pad=1)
    plt.suptitle("Task execution summary" + title_suffix)
    plt.savefig(outfile)


def plot_ray_tasks_from_config(config, events, outfile=None):
    if outfile is None:
        outfile = plotting.format_figure_filename(config, "ray_tasks.png")
    plot_ray_tasks(events, outfile, plotting.config_suffix(config))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "experiment_id", type=str, help="id or path to config.json or events.json"
    )
    parser.add_argument(
        "--outfile",
        default=None,
        help="Output file (png); default ray_tasks.png in config or current dir",
    )
    args = parser.parse_args()

    config, logs = experiment_result.load_config_events(args.experiment_id)
    if config is not None:
        plot_ray_tasks_from_config(config, logs, args.outfile)
    else:
        plot_ray_tasks(
            logs, args.outfile if args.outfile is not None else "ray_tasks.png", ""
        )


if __name__ == "__main__":
    main()

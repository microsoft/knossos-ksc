import argparse
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np

matplotlib.use("Agg")

from rlo import experiment_result
from rlo import plotting
from rlo import utils


DEFAULT_FPS = 2


def plot_empirical_animated(
    outfile, events, suffix="", log_scale=False, repetition=0, fps=DEFAULT_FPS
):
    def get_len(e):
        assert len(e["empirical_value"]) == len(e["predicted_value"])
        return len(e["empirical_value"])

    logs_by_gen = utils.group_by(
        [r for r in events if r["event"] == "plot_value_comparison"],
        lambda e: e["generation"],
    )
    max_tl = max(get_len(e) for es in logs_by_gen.values() for e in es)

    def vals(e):
        return [
            pred - emp for pred, emp in zip(e["predicted_value"], e["empirical_value"])
        ]

    fig, axs = plt.subplots(2, 1, figsize=[12, 12])
    count_axs = [ax.twinx() for ax in axs]

    def all_vals():
        yield from (v for es in logs_by_gen.values() for e in es for v in vals(e))

    min_v, max_v = min(all_vals()), max(all_vals())
    plt.suptitle("Predicted vs empirical" + suffix)

    def plot_ax(ax, count_ax, events):
        vals_by_tl = [[] for tl in range(max_tl)]
        for e in events:
            for tl, v in enumerate(vals(e)):
                vals_by_tl[tl].append(v)
        parts = ax.violinplot(
            *zip(*[(vals, tl) for tl, vals in enumerate(vals_by_tl) if len(vals) > 0]),
            range(max_tl),
            points=100,
            widths=0.8,
            showmeans=True,
            showmedians=True,
            showextrema=False,
        )
        parts["cmeans"].set_edgecolor("black")
        ax.axhline(
            np.mean([v for e in events for v in vals(e)]), color="black", linewidth=1
        )
        count_ax.plot(
            range(1, max_tl + 1), [len(v) for v in vals_by_tl], color="red", linewidth=1
        )

    def plot_gen(gen):
        print(f"Plotting gen {gen}")
        for ax in list(axs) + list(count_axs):
            ax.clear()
        logs = logs_by_gen.get(gen, [])
        if len(logs) > 0:
            plot_ax(axs[0], count_axs[0], logs)
            plot_ax(
                axs[1], count_axs[1], [e for e in logs if e["repetition"] == repetition]
            )
        elif gen > 0:
            return
        axs[0].set_title(f"Generation {gen} (all reps)")
        axs[1].set_title(f"Repetition {repetition} Generation {gen}")
        for ax in axs:
            if log_scale:
                ax.set_yscale("symlog")
            ax.set_ylabel("predicted-empirical")
            ax.set_xlabel("time_left")
            ax.set_xlim(1, max_tl)
            ax.set_ylim(min_v, max_v)
        for count_ax in count_axs:
            count_ax.set_ylabel("#points", color="red")
            count_ax.tick_params(axis="y", labelcolor="red")

    ani = animation.FuncAnimation(
        fig,
        lambda frame: plot_gen(frame + 1),
        init_func=lambda: plot_gen(0),
        frames=max(logs_by_gen.keys()),
    )
    ani.save(outfile, fps=fps, dpi=200, writer="ffmpeg")
    plt.close()


def plot_empirical_animated_from_config(
    config, events, log_scale=None, fps=DEFAULT_FPS
):
    outfile = plotting.format_figure_filename(config, "empirical_animated.mp4")
    if log_scale is None:
        log_scale = config.get("cost_normalization") not in [None, "none"]
    plot_empirical_animated(
        outfile, events, plotting.config_suffix(config), log_scale, fps
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="a run ID (e.g., 2019_01_06_13_15_48_13172) or path to a config.json file",
    )
    parser.add_argument(
        "--log_scale",
        type=bool,
        default=None,
        help="Override to specify log_scale empirical/predicted",
    )
    parser.add_argument(
        "--fps",
        type=float,
        default=DEFAULT_FPS,
        help="speed of the animation - frames per second",
    )
    args = parser.parse_args()

    config, events = experiment_result.load_config_events(args.run_id, verbosity=1)
    plot_empirical_animated_from_config(config, events, args.log_scale, args.fps)


if __name__ == "__main__":
    main()

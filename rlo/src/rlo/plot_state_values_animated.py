import matplotlib
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np
from textwrap import wrap
import argparse

matplotlib.use("Agg")

from rlo.factory import num_time_heads_from_config
from rlo import experiment_result
from rlo import plotting
from rlo import utils


def plot_state_values_animated(
    outfile, events, num_time_heads, suffix="", log_scale=False, fps=1
):
    def to_log_scale(v):
        return np.log(v + 0.0001)

    y_transform = to_log_scale if log_scale else lambda y: y
    expr_value_info_events = [e for e in events if e["event"] == "expr_value_info"]
    group_by_gen = utils.group_by(expr_value_info_events, lambda r: r["generation"])

    # get max and min of percentiles to set axis limits
    value_perc_max, value_perc_min = np.float("-inf"), np.float("inf")
    for eg in group_by_gen.values():
        for ex in utils.group_by(eg, lambda r: r["eval_expr"]).values():
            value_perc = np.nanpercentile(
                [[y_transform(v) for v in e["value"]] for e in ex], [10, 50, 90], axis=0
            )
            value_perc_max = max(np.max(value_perc), value_perc_max)
            value_perc_min = min(np.min(value_perc), value_perc_min)

    N_GENERATIONS = max(r["generation"] for r in expr_value_info_events)
    MAX_V = value_perc_max
    MIN_V = value_perc_min

    fig, ax = plt.subplots(figsize=(12, 10))
    box = ax.get_position()
    ax.set_position(
        [box.x0, box.y0, box.width * 0.8, box.height]
    )  # make room for legend to right
    plt.xlabel("time left", fontsize=16)
    plt.ylabel("value", fontsize=16)

    def plot_by_gen(gen_num):
        ax.clear()
        if gen_num not in group_by_gen.keys():
            return

        group_by_exp = utils.group_by(group_by_gen[gen_num], lambda r: r["eval_expr"])

        # plot value predictions for each of the expressions
        for expr, expr_events in group_by_exp.items():
            assert len(set([e["repetition"] for e in expr_events])) == len(expr_events)
            value = [[y_transform(v) for v in e["value"]] for e in expr_events]

            plotting.plot_with_confidence(
                ax,
                range(num_time_heads),
                np.array(value),
                plotting.random_color_from_string(expr),
                label=expr,
            )

        # plot x-axis
        ax.plot(range(num_time_heads), [0] * num_time_heads, color="black", linewidth=2)

        title_suffix = "\n".join(
            wrap(
                "Expected improvement for different time budgets, gen {}.{}".format(
                    gen_num, suffix
                ),
                120,
            )
        )
        ax.legend(
            *plotting.combine_legend_handles_labels(ax),
            loc="center left",
            bbox_to_anchor=(1, 0.5),
            fontsize=8
        )
        ax.set_title(title_suffix)
        ax.set_ylim(-0.1 + MIN_V, MAX_V + 0.1)
        ax.grid()

    def init():
        plot_by_gen(0)

    ani = animation.FuncAnimation(
        fig, plot_by_gen, init_func=init, frames=N_GENERATIONS + 1
    )
    ani.save(outfile, fps=fps, dpi=200, writer="ffmpeg")
    plt.close()


def plot_state_values_animated_from_config(config, events):
    outfile = plotting.format_figure_filename(config, "state_values_animated.mp4")
    log_scale = True if config.get("cost_normalization") is not None else False
    num_time_heads = num_time_heads_from_config(config)
    plot_state_values_animated(
        outfile, events, num_time_heads, plotting.config_suffix(config), log_scale,
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
        action="store_true",
        help="whether to use log scale for plotting",
    )
    parser.add_argument(
        "--fps",
        type=float,
        default=1,
        help="speed of the animation - frames per second",
    )
    args = parser.parse_args()

    config, events = experiment_result.load_config_events(args.run_id)
    outfile = plotting.format_figure_filename(config, "state_values_animated.mp4")
    num_time_heads = num_time_heads_from_config(config)
    plot_state_values_animated(
        outfile,
        events,
        num_time_heads,
        plotting.config_suffix(config),
        args.log_scale,
        args.fps,
    )


if __name__ == "__main__":
    main()

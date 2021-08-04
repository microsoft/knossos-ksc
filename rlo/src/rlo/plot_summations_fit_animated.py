# mypy: ignore-errors
import argparse
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np
from rlo import sparser

# Collection is used to indicate something which can be iterated through repeatedly, but where the order is not significant;
# and Sequence for where the order/position is significant (even if we may not index into it).
from typing import Callable, Collection, Sequence, Tuple, Optional

matplotlib.use("Agg")

from rlo import experiment_result
from rlo.summations import SummationsExpert
from rlo import plotting
from rlo import utils

DEFAULT_FPS = 2


def flatten(seqs):
    for seq in seqs:
        yield from seq


def plot_hist(ax, tgt_then_fitted: Sequence[Sequence[Tuple[float, float]]], tgt_type):
    fitted_for_tgt = {k: [] for k in range(3)}
    for tgt, fitted in flatten(tgt_then_fitted):
        # We use NaN to exclude particular time-lefts from the expert plots.
        if not np.isnan(tgt):
            fitted_for_tgt[tgt].append(fitted)
    for tgt, fitteds in fitted_for_tgt.items():
        ax.hist(fitteds, histtype="step", label=f"{tgt_type} value {tgt}")
    ax.set_xlabel("Fitted value")
    ax.set_xlim(-0.3, 2.3)
    ax.set_ylabel("#points")
    ax.set_yscale("log")
    ax.set_ylim(1, 1e5)
    ax.legend(loc="upper center")


def group_by_target_then_tl(
    tgt_and_fitted_per_expr: Collection[Sequence[Tuple[float, float]]]
) -> Sequence[Sequence[Sequence[float]]]:
    """ Input: a collection (index not significant) of
            sequences (indexed by time-left) of
                pairs of (target value, fitted value) - NaN targets to be skipped
        Output - a sequence (where index == target value i.e. 0,1,2) of
            sequences (indexed by time-left) of
                collections (index not significant) of
                    fitted values
    """
    max_len = max(len(seq) for seq in tgt_and_fitted_per_expr)

    tgt_and_fitted_by_tl = [[] for tl in range(max_len)]
    for seq in tgt_and_fitted_per_expr:
        for tl, val_at_tl in enumerate(seq):
            tgt_and_fitted_by_tl[tl].append(val_at_tl)
    return [
        [
            [fitted for tgt, fitted in tgt_and_fitteds if tgt == tgt_val]
            for tgt_and_fitteds in tgt_and_fitted_by_tl
        ]
        for tgt_val in range(3)
    ]


def plot_violins(ax, fitted_by_tgt_then_tl: Sequence[Sequence[Collection[float]]]):
    for tgt_val, fitted_by_tl in enumerate(fitted_by_tgt_then_tl):
        vals_with_tl = [(tl, vs) for tl, vs in enumerate(fitted_by_tl) if len(vs) > 0]
        if len(vals_with_tl) > 0:
            xs_with_offset, vals = zip(
                *[(tl + (tgt_val / 3), vs) for tl, vs in vals_with_tl]
            )
            ax.violinplot(
                vals,
                xs_with_offset,
                points=100,
                widths=0.4,
                showmeans=False,
                showmedians=False,
                showextrema=False,
            )
    ax.set_ylabel("Fitted value")
    ax.set_ylim(-0.3, 2.3)
    ax.set_xlabel("Time_left")


def plot_counts(count_ax, fitted_by_tgt_then_tl: Sequence[Sequence[Collection[float]]]):
    counts_by_tgt_then_tl = np.array(
        [
            [len(pts_for_tl) for pts_for_tl in fitted_by_tl]
            for fitted_by_tl in fitted_by_tgt_then_tl
        ]
    )
    counts_by_tl = counts_by_tgt_then_tl.sum(axis=0)  # Sum all target values
    xs = range(1, len(counts_by_tl) + 1)
    for tgt_val, counts_for_tgt in enumerate(counts_by_tgt_then_tl):
        count_ax.plot(xs, counts_for_tgt, label=f"#points for {tgt_val}")
    count_ax.plot(xs, counts_by_tl, color="black", label="total #points")


def plot_row(
    row,
    tgt_and_fitted_per_expr: Collection[Sequence[Tuple[float, float]]],
    tgt_type: str,
):
    ax1, ax2, count_ax = row
    plot_hist(ax1, tgt_and_fitted_per_expr, tgt_type)
    fitted_by_tgt_then_tl = group_by_target_then_tl(tgt_and_fitted_per_expr)
    plot_violins(ax2, fitted_by_tgt_then_tl)
    plot_counts(count_ax, fitted_by_tgt_then_tl)


def plot_summations_fit_animated(
    outfile,
    events,
    expert_fn: Callable[[int], Optional[SummationsExpert]] = lambda _: None,
    repetition=0,
    suffix="",
    fps=DEFAULT_FPS,
):
    """ expert_fn: function from max_depth to optional expert. """
    fit_events = [
        e
        for e in events
        if e["event"] == "distill_fit" and e["repetition"] == repetition
    ]
    target_and_fitteds_by_gen = {
        gen: utils.single_elem(evt)["target_and_fitted"]
        for gen, evt in utils.group_by(fit_events, lambda e: e["generation"]).items()
    }
    max_num_exprs = max(len(vs) for vs in target_and_fitteds_by_gen.values())

    expert = expert_fn(
        max(len(seq) for seq in flatten(target_and_fitteds_by_gen.values()))
    )
    rows = 1 if expert is None else 4
    fig, axss = plt.subplots(rows, 2, figsize=(16, 8 * rows), squeeze=False)
    rows = [list(axs) + [axs[1].twinx()] for axs in axss]

    def plot_gen(gen_num):
        for ax in flatten(rows):
            ax.clear()
        for _1, _2, count_ax in rows:
            count_ax.set_ylim(1, max_num_exprs)
        if gen_num not in target_and_fitteds_by_gen.keys():
            print("Skipping ", gen_num)
            return
        print("Plotting ", gen_num)
        target_and_fitted_by_expr = target_and_fitteds_by_gen[gen_num]

        # Top plots: fitted vs empirical (target value on which model just trained)
        plot_row(rows[0], target_and_fitted_by_expr.values(), "Empirical")

        if len(rows) > 1:
            # Bottom plots use expert values
            target_expert_fittedss = [
                [
                    (target, expert, fitted)
                    for expert, (target, fitted) in zip(
                        utils.single_elem(
                            expert.evaluate_all_time_left([sparser.parse_expr(expr)])
                        ),
                        target_and_fitteds,
                    )
                ]
                for expr, target_and_fitteds in target_and_fitted_by_expr.items()
            ]

            def get_seqs(target_fn):
                return [
                    [
                        (target_fn(target, expert), fitted)
                        for target, expert, fitted in seq
                    ]
                    for seq in target_expert_fittedss
                ]

            # fitted values for target values less than the expert
            plot_row(
                rows[1],
                get_seqs(
                    lambda target, expert: target if target < expert else float("nan")
                ),
                "Empirical (< expert)",
            )
            # fitted values for target values equal to the expert
            plot_row(
                rows[2],
                get_seqs(
                    lambda target, expert: target if target == expert else float("nan")
                ),
                "Empirical ()== expert)",
            )
            # fitted values against expert values
            plot_row(
                rows[3], get_seqs(lambda _target, expert: expert), "Expert",
            )

        plt.suptitle(f"Generation {gen_num} summations fit {suffix}")

    ani = animation.FuncAnimation(
        fig,
        plot_gen,
        init_func=lambda: plot_gen(0),
        frames=range(
            min(target_and_fitteds_by_gen.keys()), max(target_and_fitteds_by_gen.keys())
        ),
    )
    ani.save(outfile, fps=fps, dpi=200, writer="ffmpeg")
    plt.close()


def plot_summations_expert_fit_from_config(config, events, **_kwargs):
    """This is a verbose plot. Provide with verbose_events"""

    plot_summations_fit_animated(
        plotting.format_figure_filename(config, "summations_expert_fit.mp4"),
        events,
        suffix=plotting.config_suffix(config),
        expert_fn=SummationsExpert,
    )


def plot_summations_fit_animated_from_config(config, events, **_kwargs):
    """This is a verbose plot. Provide with verbose_events"""

    plot_summations_fit_animated(
        plotting.format_figure_filename(config, "summations_fit.mp4"),
        events,
        suffix=plotting.config_suffix(config),
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="a run ID (e.g., 2019_01_06_13_15_48_13172) or path to a config.json file",
    )
    parser.add_argument(
        "--fps",
        type=float,
        default=DEFAULT_FPS,
        help="speed of the animation - frames per second",
    )
    parser.add_argument(
        "--repetition", type=int, default=0, help="which repetition to plot"
    )
    parser.add_argument(
        "--expert",
        action="store_true",
        help="include plots of datapoints grouped by expert value (slow!)",
    )
    args = parser.parse_args()

    config, events = experiment_result.load_config_events(args.run_id, verbosity=1)
    outfile = plotting.format_figure_filename(
        config, "summations_expert_fit.mp4" if args.expert else "summations_fit.mp4"
    )

    plot_summations_fit_animated(
        outfile,
        events,
        repetition=args.repetition,
        suffix=plotting.config_suffix(config),
        expert_fn=SummationsExpert if args.expert else lambda _: None,
        fps=args.fps,
    )


if __name__ == "__main__":
    main()

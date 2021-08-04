import argparse
import matplotlib
import matplotlib.pyplot as plt
from pandas import DataFrame
from textwrap import wrap
from typing import Dict, Iterable, Literal
import numpy as np
import scipy.stats

matplotlib.use("Agg")

from rlo import experiment_result
from rlo.sparser import parse_expr
from rlo import plotting
from rlo.expression import Expression


def plot_scatter(
    df: DataFrame,
    ax: plt.Axes,
    color_key: str,
    title: str,
    logscale: bool,
    cmap,
    min_point: float,
    max_point: float,
    jitter_amount: float = 0.0,
) -> None:
    def correct_zero_neg_if_logscale(v: np.ndarray, logscale: bool) -> np.ndarray:
        if not logscale:
            return v
        else:
            return np.clip(v, 0.001, None)

    # Scatter target against fitted values (possibly with some jitter)
    target = correct_zero_neg_if_logscale(
        add_jitter(df["target"], jitter_amount=jitter_amount, log_scale=logscale),
        logscale=logscale,
    )
    fitted = correct_zero_neg_if_logscale(df["fitted"], logscale=logscale)
    sc = ax.scatter(target, fitted, c=df[color_key], cmap=plt.get_cmap(cmap), alpha=0.7)

    # Plot y = x, x = 1 and y = 1 lines for visual reference
    ax.plot([min_point, max_point], [min_point, max_point], c="gray")
    ax.axvline(1, color="gray", linewidth=0.5)
    ax.axhline(1, color="gray", linewidth=0.5)

    ax.set_title(title, fontsize=20)
    if logscale:
        ax.set_xscale("log")
        ax.set_yscale("log")
    ax.set_xlim((min_point, max_point))
    ax.set_ylim((min_point, max_point))
    ax.set_xlabel("target", fontsize=20)
    ax.set_ylabel("fitted", fontsize=20)
    ax.xaxis.set_tick_params(labelsize=20)
    ax.yaxis.set_tick_params(labelsize=20)
    plt.colorbar(sc, ax=ax)

    # Add goodness-of-fit metrics to the plot:
    mse = np.mean((df["target"] - df["fitted"]) ** 2)
    pearson_r = df["target"].corr(df["fitted"])
    spearman_corr = scipy.stats.spearmanr(df["target"], df["fitted"]).correlation
    ax.text(
        x=0.1,
        y=0.95,
        s=f"Pearson-R: {pearson_r:.3f}, MSE: {mse:.3f}, Spearman: {spearman_corr:.3f}",
        transform=ax.transAxes,  # x, y specified in axis-"percentage" coordinates
        fontsize=20,
    )


def add_jitter(
    data: np.ndarray, jitter_amount: float, log_scale: bool = False,
) -> np.ndarray:
    if log_scale:
        return data * np.random.uniform(
            1 - jitter_amount, 1 + jitter_amount, size=data.shape
        )
    else:
        return data + np.random.uniform(-jitter_amount, jitter_amount, size=data.shape)


def plot_scatter_fitted_from_events(
    outfile: str,
    distill_fit_events: Iterable,
    train_valid_info: Dict[Expression, Literal["train", "valid"]],
    suffix: str = "",
    jitter_amount: float = 0.4,
    logscale: bool = False,
    cmap="gist_rainbow",
) -> None:
    """Scatter plot of predicted "fitted" values vs. the true target values.

    Args:
        outfile: Path to the file where to save this plot.
        distill_fit_events: Iterable of events (see analytics.py for more info) of type
            "distill_fit"
        train_valid_info: Dictionary from expressions to either "train" or "valid"
            indicating whether the expression belong to train or valid set.
        suffix: Suffix for the filename of the saved plot
        jitter_amount (optional): Adds uniform random noise to the target values to help make the volume of
            "point clouds" clearer. There doesn't seem much point in adding noise to the fitted
            values tho as they already have substantial spread. Defaults to 0.4.
        logscale (optional): . Defaults to False.
        cmap (optional): . Defaults to "gist_rainbow".
    """
    for event in distill_fit_events:
        df = DataFrame(
            [
                {
                    "time_left": tl,
                    "target": targ,  #  target can be 0
                    "fitted": fit,
                    "nodes": parse_expr(expr).num_nodes,
                    "phase": train_valid_info[expr],
                }
                for expr, seq in event["target_and_fitted"].items()
                for tl, (targ, fit) in enumerate(seq[1:], 1)
            ]
        )

    total_num_points = len(df)
    num_negative = (df["fitted"] <= 0).sum()
    print(f"Got {total_num_points} items, {num_negative} fitted values were negative")
    min_point = min(min(df.target), min(df.fitted))
    max_point = max(max(df.target), max(df.fitted))

    fig, axss = plt.subplots(2, 2, figsize=(30, 24))
    for axs, phase in zip(axss, ["train", "valid"]):
        for ax, color_key in zip(axs, ["time_left", "nodes"]):
            plot_scatter(
                df=df[df["phase"] == phase],
                ax=ax,
                color_key=color_key,
                title=f"By {color_key} ({phase} split)",
                logscale=logscale,
                cmap=cmap,
                min_point=min_point,
                max_point=max_point,
                jitter_amount=jitter_amount,
            )

    fig.suptitle(
        "\n".join(
            wrap(
                "Fitted vs target values, num_points={} ".format(total_num_points)
                + suffix,
                150,
            )
        ),
        fontsize=20,
    )
    plt.savefig(outfile)


def plot_scatter_fitted_from_config(
    config,
    events,
    repetition=0,
    generation=1,
    cmap="gist_rainbow",
    logscale=None,
    target_jitter=None,
):
    if logscale is None:
        logscale = config.get("cost_normalization") not in ["none", None]
    if target_jitter is None:
        target_jitter = config.get("scatter_target_jitter", 0.0)

    # Create a dict to allow for looking up if an expression belong to
    # "train" or "valid" data
    train_valid_info: Dict[Expression, Literal["train", "valid"]] = {}
    for e in events:
        if e.get("repetition") != repetition or e.get("generation") != generation:
            continue
        if e["event"] == "distill_train_split":
            train_valid_info.update({expr: "train" for (expr, _) in e["dataset"]})
        elif e["event"] == "distill_valid_split":
            train_valid_info.update({expr: "valid" for (expr, _) in e["dataset"]})

    distill_fit_events = (
        e
        for e in events
        if (
            e.get("event") == "distill_fit"
            and e["repetition"] == repetition
            and e["generation"] == generation
        )
    )
    plot_scatter_fitted_from_events(
        outfile=plotting.format_figure_filename(
            config, "scatter_fitted_{}_{}.png".format(repetition, generation)
        ),
        distill_fit_events=distill_fit_events,
        train_valid_info=train_valid_info,
        suffix=f"(repetition {repetition}, generation {generation})"
        + plotting.config_suffix(config),
        jitter_amount=target_jitter,
        logscale=logscale,
        cmap=cmap,
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
        action="store_true",
        help="whether to use log scale for target and fitted values",
    )
    parser.add_argument(
        "--target_jitter",
        type=float,
        default=None,
        help="Jitter target amounts by +/- (specified amount, or fraction if logscale)",
    )
    parser.add_argument(
        "--repetition", type=int, default=0, help="Which repetition to plot (default 0)"
    )
    parser.add_argument(
        "--generation", type=int, default=1, help="Which generation to plot (default 1)"
    )
    parser.add_argument(
        "--cmap", type=str, default="gist_rainbow", help="Which colormap to use"
    )
    args = parser.parse_args()

    config, events = experiment_result.load_config_events(args.run_id, verbosity=1)
    plot_scatter_fitted_from_config(
        config=config,
        events=events,
        repetition=args.repetition,
        generation=args.generation,
        cmap=args.cmap,
        logscale=args.log_scale,
        target_jitter=args.target_jitter,
    )


if __name__ == "__main__":
    main()

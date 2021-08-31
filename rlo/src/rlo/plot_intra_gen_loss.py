"""
Make a plot of the training/validation losses during the distillation (model-fitting) phase of a run for a 
particular generation and a particular repetition.

Note that running this script required logs of verbosity >= 1, i.e. if they've been pre-downloaded before running
this script, they should be downloaded with the `verbosity=1` flag:
    experiment_result.load_config_events(run_id, azure_ml_run_idt, verbosity=1)
"""

import argparse
import math
from operator import itemgetter
from typing import Callable, Dict, Iterable, List, Optional, Sequence, Tuple

import matplotlib

matplotlib.use("Agg")
import matplotlib.animation as animation
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt

from rlo import experiment_result, plotting, utils
from rlo.flags import ConfigType
from rlo.tf_model import DualModelWrapper


def verify_plot_events_correct_epoch(events) -> None:
    """Verify that the iteration numbers of train/validation events look as expected."""

    for epoch, event in enumerate(events):
        assert event["epoch_counter"] == epoch


def plot_intra_gen_loss_one_generation(
    events: Iterable,
    save_file: str,
    generation: int,
    num_repetitions: int,
    title: str = "Loss",
    per_batch: bool = False,
    axis_size: float = 5.0,
    event_filter: Optional[Callable[[Dict], bool]] = None,
) -> None:
    # If not event-filter given, default to an always true filter
    event_filter = event_filter or (lambda x: True)
    # Get all distill_epoch events for this generation
    distill_events = [
        e
        for e in events
        if (
            e["event"] == "distill_epoch"
            and e["generation"] == generation
            and event_filter(e)
        )
    ]

    fig, axes, _ = _make_intra_gen_loss_axes(
        num_repetitions=num_repetitions, axis_size=axis_size, add_legend_ax=False
    )

    # Extra axis limits (to be used if doing a per_epoch plot)
    xaxis_max, yaxis1_min, yaxis1_max = _extract_axis_limits(distill_events)

    plot_intra_gen_loss_multiple_repetitions(
        events=distill_events,
        axes=axes,
        per_batch=per_batch,
        log_scale=True,
        xaxis_limits=(0, xaxis_max) if not per_batch else None,
        yaxis_limits=(yaxis1_min, yaxis1_max) if not per_batch else None,
        legend_ax=None,
    )

    fig.suptitle(title, fontsize=16)
    fig.savefig(save_file, bbox_inches="tight")
    plt.close()


def plot_intra_gen_loss_animated(
    save_file: str,
    events: Iterable,
    num_repetitions: int,
    fps: float = 1.0,
    axis_size: float = 5.0,
    event_filter: Optional[Callable[[Dict], bool]] = None,
) -> None:
    # If not event-filter given, default to an always true filter
    event_filter = event_filter or (lambda x: True)
    # Filter out events that are not a "distill_epoch" event
    events = [
        e
        for e in events
        if (
            e["event"] == "distill_epoch"
            and e["repetition"] <= num_repetitions
            and event_filter(e)
        )
    ]

    fig, axes, legend_ax = _make_intra_gen_loss_axes(
        num_repetitions=num_repetitions, axis_size=axis_size, add_legend_ax=True,
    )

    xaxis_max, yaxis1_min, yaxis1_max = _extract_axis_limits(events)
    # Group the events by generation
    group_by_gen: Dict[int, List] = utils.group_by(events, itemgetter("generation"))

    def plot_by_gen(gen_number: int) -> None:
        if gen_number not in group_by_gen.keys():
            return
        plot_intra_gen_loss_multiple_repetitions(
            events=group_by_gen[gen_number],
            axes=axes,
            per_batch=False,
            log_scale=True,
            xaxis_limits=(0, xaxis_max),
            yaxis_limits=(yaxis1_min, yaxis1_max),
            legend_ax=legend_ax,
        )
        # Set figure title
        fig.suptitle(f"Generation {gen_number}")

    # Make the animation
    num_generations = max(group_by_gen.keys())
    ani = animation.FuncAnimation(
        fig, plot_by_gen, frames=range(1, num_generations + 1),
    )
    ani.save(save_file, fps=fps, dpi=200, writer="ffmpeg")
    plt.close()


def _extract_axis_limits(events: Iterable) -> Tuple[int, float, float]:
    """
    Extract limits across repetitions (and possibly generations) for each axis for the intra_gen_loss_per_epoch plot.
    """
    # Get the [min, max] of each loss to set axis limits for the y-axis:
    # yaxis1: training and validation losses share one axis (left), so extract the range of both of them jointly

    # relevant_metrics is a list of tuples of (train_loss, valid_loss, per_batch_mean_train_loss, epoch_counter)
    relevant_metrics = [
        (event["train_loss"], event["valid_loss"], event["epoch_counter"],)
        for event in events
    ]

    train_loss_min = min(relevant_metrics, key=itemgetter(0))[0]
    valid_loss_min = min(relevant_metrics, key=itemgetter(1))[1]

    train_loss_max = max(relevant_metrics, key=itemgetter(0))[0]
    valid_loss_max = max(relevant_metrics, key=itemgetter(1))[1]
    epoch_counter_max = max(relevant_metrics, key=itemgetter(2))[2]

    yaxis1_min = min(train_loss_min, valid_loss_min)
    yaxis1_max = max(train_loss_max, valid_loss_max)
    xaxis_max = epoch_counter_max

    return xaxis_max, yaxis1_min, yaxis1_max


def _make_intra_gen_loss_axes(
    num_repetitions: int, axis_size: float, add_legend_ax: bool = False
) -> Tuple[plt.Figure, List[plt.Axes], Optional[plt.Axes]]:
    """Make a grid of axes - one for each repetition - arranged in a rectangle."""
    # Make a separate axis for each repetition, arranging them in a rectangle
    num_axes = num_repetitions + int(add_legend_ax)
    num_cols = math.ceil(num_axes ** 0.5)
    num_rows = math.ceil(num_axes / num_cols)
    fig = plt.figure(
        constrained_layout=True,
        figsize=(axis_size * num_cols * 1.2, axis_size * num_rows),
    )
    spec = gridspec.GridSpec(ncols=num_cols, nrows=num_rows, figure=fig)
    # Add an axis for figure for each repetition
    axes = []
    for repetition in range(num_repetitions):
        # Repetition axes arranged left-to-right, top-to-bottom
        repetition_ax = fig.add_subplot(
            spec[repetition // num_cols, repetition % num_cols]
        )
        axes.append(repetition_ax)

    if add_legend_ax:
        legend_ax = fig.add_subplot(spec[-1, -1])
        legend_ax.axis("off")
    else:
        legend_ax = None

    return fig, axes, legend_ax


def plot_intra_gen_loss_multiple_repetitions(
    events: Iterable,
    axes: List[plt.Axes],
    per_batch: bool = False,
    log_scale: bool = False,
    xaxis_limits: Optional[Tuple[float, float]] = None,
    yaxis_limits: Optional[Tuple[float, float]] = None,
    legend_ax: Optional[plt.Axes] = None,
) -> None:
    """Plot train (+ possibly validation) losses on a grid of axes, one axis for each repetition.

    Note: events assumed to only have events of type "distill_epoch" and all events are assumed to correspond
    to one generation

    Args:
        events: An iterable of event dictionaries, all for one generation only, and all of "distill_epoch" type.
        axes: List of axes of same length as the number of repetition to plot
        per_batch (optional): Whether to plot losses per batch (rather than per epoch). Defaults to False.
        log_scale (optional): Whether to make yaxis log-scale. Defaults to False.
        xaxis_limits (optional): Limits for the x-axis for all the axes. Defaults to None
        yaxis_limits (optional): Limits for the y-axis for all the axes. Defaults to None.
        legend_ax (optional): If given, the legend will be made on this axis rather than below plot.
    """
    # Group events by repetition:
    group_by_repetition = utils.group_by(events, itemgetter("repetition"))

    assert len(group_by_repetition) != 0, "At least 1 repetition required."

    for repetition, ax in enumerate(axes):
        # Clear axes (we're reusing the same figure for animation)
        ax.clear()

        if repetition in group_by_repetition:
            # Sometimes different repetitions run for different number of generations.
            # If plotting for a gen. that doesn't have this repetition, skip plotting
            # the curves.
            _plot_intra_gen_loss_from_events(
                events=group_by_repetition[repetition],
                ax=ax,
                log_scale=log_scale,
                add_legend=False,
                per_batch=per_batch,
            )
        # Set axis limits
        if xaxis_limits:
            ax.set_xlim(*xaxis_limits)
        if yaxis_limits:
            ax.set_ylim(*yaxis_limits)

        # Set title
        ax.set_title(f"Repetition {repetition}")

    # Add a figure legend (if not making a per-batch plot):
    if not per_batch:
        # Get axis for a repetition that was plotted (to get line handles for legend)
        plotted_repetition = next(iter(group_by_repetition.keys()))
        used_axis = axes[plotted_repetition]

        handles, labels = used_axis.get_legend_handles_labels()
        if legend_ax:
            # If a legend ax given, plot on that axis
            legend_ax.legend(handles, labels)
        else:
            # Otherwise plot below all plots
            plt.figlegend(
                handles,
                labels,
                loc="upper center",
                bbox_to_anchor=(0.5, -0.13),
                ncol=len(handles),
            )


def _plot_intra_gen_loss_from_events(
    events: Iterable,
    ax: plt.Axes,
    per_batch: bool = False,
    add_legend: bool = True,
    log_scale: bool = False,
) -> None:
    """Plot intra-generation training and validation losses (for a single repetition),
    extracting them from events.

    Note: The passed events should all have event type "distill_epoch"!

    Args:
        events: An iterable of event dictionaries (see `analytics.py`).
            All events assumed to be of type "distill_epoch".
        ax: Axis on which to plot the losses
        per_batch (optional): Whether to make a per-batch plot rather than a per-epoch one. Defaults to False.
        add_legend (optional): Whether to put a legend in the corner. Defaults to True.
        log_scale (optional): Whether to make the y-axis in log-scale. Defaults to False.
    """
    # Sort events by epoch
    events = sorted(events, key=itemgetter("epoch_counter"))

    # Make sure that the events look valid for plotting.
    verify_plot_events_correct_epoch(events)

    # Get a list of lists of losses per batch
    train_losses_per_batch_for_each_epoch: List[List[float]] = [
        event["train_losses"] for event in events
    ]

    ax.grid(True)

    if per_batch:
        # Flatten the list of lists of floats into a list of floats
        train_losses_per_batch: List[float] = sum(
            train_losses_per_batch_for_each_epoch, []
        )

        # Make the plot
        _plot_loss_per_batch(train_losses_per_batch, ax=ax, log_scale=log_scale)
    else:
        train_losses = [event["train_loss"] for event in events]
        valid_losses = [event["valid_loss"] for event in events]
        epochs = [event["epoch_counter"] for event in events]
        # Plot the training and validation losses per epoch
        _plot_loss_per_epoch(
            epochs=epochs,
            train_losses=train_losses,
            valid_losses=valid_losses,
            ax=ax,
            log_scale=log_scale,
        )

        if add_legend:
            ax.legend(*ax.get_legend_handles_labels(), loc=1)


def _plot_loss_per_batch(
    train_losses_per_batch: Sequence[float], ax: plt.Axes, log_scale: bool = False,
) -> None:
    ax.plot(train_losses_per_batch)
    ax.set_ylabel("Batch training loss")
    ax.set_xlabel("Batch number")
    ax.set_yscale("log" if log_scale else "linear")


def _plot_loss_per_epoch(
    epochs: Sequence[float],
    train_losses: Sequence[float],
    valid_losses: Sequence[float],
    ax: plt.Axes,
    log_scale: bool = False,
) -> Tuple[plt.Axes]:
    """Plots training/validations losses per epoch on a single axis.

    Args:
        epochs: Sequence of epochs at which train/valid/train-mean-per-batch losses have been measured
        train_losses: The training loss sequence of same length as epochs
        valid_losses: The validation loss sequence of same length as epochs
        ax: The axis on which to plot the curves.
        log_scale (optional): Whether to set the y-axes to be in log-scale. Defaults to False.
    """
    ax.plot(
        epochs,
        train_losses,
        label="Training loss",
        marker="o",
        color=plotting.colors(0),
        zorder=2,
    )
    ax.plot(
        epochs,
        valid_losses,
        label="Validation loss",
        marker="x",
        color=plotting.colors(1),
        linestyle="dashed",
    )
    # Color y-axis to visually convey which ticks correspond to which curves
    ax.tick_params(
        axis="y", which="both", colors=plotting.colors(0)
    )  # which='both' -> change minor *and* major ticks
    # Set axis limits/scales and decorate
    if log_scale:
        ax.set_yscale("log")
    else:
        ax.set_yscale("linear")
        ax.set_ylim(bottom=0)

    ax.set_xlim(epochs[0], epochs[-1])
    ax.set_xlabel("epoch")
    return ax


def validate_args(args: argparse.Namespace) -> None:
    # Can't specify --per_batch and not specify a generation:
    if args.per_batch and args.generation is None:
        raise ValueError(
            "If `--per_batch` specified, need to specify a generation (with `--generation`) to plot as well. "
            "Can't make the per_batch plot into a video."
        )


def plot_intra_gen_loss_from_config(
    config: ConfigType,
    events: Iterable,
    generation: Optional[int] = None,
    per_batch: bool = False,
) -> None:
    if generation is not None:
        plot_intra_gen_loss_one_generation_from_config(
            config, events, generation=generation, per_batch=per_batch
        )
    else:
        plot_intra_gen_loss_animated_from_config(config, events)


def get_model_names_and_model_name_filters(
    config: Dict,
) -> Tuple[List[str], List[Callable[[Dict], bool]]]:
    """
    Depending on whether an ensemble (two-value-func) or a single model was trained, 
    the event log might contain training events for different models. This helper 
    method returns a sequence of model names and filter functions for the event log 
    to extract only the events for each model.
    """
    if config["two_value_func"] is not None:
        model_names = [
            DualModelWrapper.first_model_name,
            DualModelWrapper.second_model_name,
        ]
        model_name_filters = [
            lambda event: event.get("distill_net", None) == model_name
            for model_name in model_names
        ]
    else:
        # Only one model in events
        model_names = [""]
        model_name_filters = [lambda event: True]
    return model_names, model_name_filters


def plot_intra_gen_loss_one_generation_from_config(
    config: ConfigType, events: Iterable, generation: int, per_batch: bool = False,
) -> None:
    # If multiple distinct models were trained as part of this run (e.g.
    # two-value-functions), make a separate loss curve plot for each of them.
    model_names, model_name_filters = get_model_names_and_model_name_filters(config)

    for model_name, model_name_filter in zip(model_names, model_name_filters):

        plot_title_suffix = plotting.config_suffix(config)
        output_file_suffix = f"_{model_name}" if model_name else ""
        output_file = plotting.format_figure_filename(
            config, f"loss_generation={generation}{output_file_suffix}.png"
        )
        plot_intra_gen_loss_one_generation(
            events=events,
            save_file=output_file,
            generation=generation,
            num_repetitions=config["num_repetitions"],
            title=f"Loss in generation {generation}; {plot_title_suffix}",
            per_batch=per_batch,
            # Only plot losses for one of the models at a time
            event_filter=model_name_filter,
        )


def plot_intra_gen_loss_animated_from_config(config: Dict, events: Iterable) -> None:
    # If multiple distinct models were trained as part of this run (e.g.
    # two-value-functions), make a separate loss curve plot for each of them.
    model_names, model_name_filters = get_model_names_and_model_name_filters(config)

    for model_name, model_name_filter in zip(model_names, model_name_filters):
        output_file_suffix = f"_{model_name}" if model_name else ""
        outfile = plotting.format_figure_filename(
            config, f"intra_gen_loss_animated{output_file_suffix}.mp4"
        )
        plot_intra_gen_loss_animated(
            save_file=outfile,
            events=events,
            num_repetitions=config["num_repetitions"],
            # If events are for an ensemble, only plot the training curves for the first model
            event_filter=model_name_filter,
        )


def get_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "run_id",
        type=str,
        help="run ID (e.g., 2019_01_06_13_15_48_13172), path to config.json, or directory containing rep/events_1.json files",
    )

    parser.add_argument(
        "--generation",
        type=int,
        default=None,
        help="Generation to plot. If none specified, all generations will be plotted as frames in a video",
    )
    parser.add_argument(
        "--per_batch",
        action="store_true",
        help="show loss per batch, instead of per pass through the dataset",
    )
    return parser


def main():
    parser = get_parser()
    args = parser.parse_args()
    validate_args(args)

    config, events = experiment_result.load_config_events(args.run_id, verbosity=1)

    plot_intra_gen_loss_from_config(
        config=config,
        events=events,
        generation=args.generation,
        per_batch=args.per_batch,
    )


if __name__ == "__main__":
    main()

import argparse
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

from rlo import experiment_result
from rlo import plotting
from rlo import utils


def _plot_losses(ax, x_vals, train_loss, valid_loss):
    """Plot extracted loss data for a single model. See _plot_model_logs."""
    plotting.plot_with_confidence(
        ax, x_vals, train_loss, plotting.colors(0), label="train"
    )
    plotting.plot_with_confidence(
        ax, x_vals, valid_loss, plotting.colors(1), label="valid"
    )
    ax.set_ylabel("Train/valid loss")
    ax.legend(loc="upper right")


def _plot_epochs(ax, x_vals, epoch_counters, best_valid_epoch):
    """Plot epoch counts and selected (best-valid) epoch for a single model."""
    plotting.plot_with_confidence(
        ax, x_vals, epoch_counters, plotting.colors(4), label="epoch_counter"
    )
    plotting.plot_with_confidence(
        ax, x_vals, best_valid_epoch, plotting.colors(5), label="best_valid_epoch",
    )
    ax.set_ylabel("#epochs")
    ax.set_ylim(bottom=0)
    ax.legend(loc="upper right")


def _plot_total_weight(ax, x_vals, total_weight):
    """Plot extracted total training weight data for a single model."""
    plotting.plot_with_confidence(
        ax, x_vals, total_weight, plotting.colors(2), label="total_weight"
    )
    ax.set_ylabel("total_weight", color=plotting.colors(2))
    ax.tick_params(axis="y", labelcolor=plotting.colors(2))
    ax.set_ylim(bottom=0)


def _plot_minibatches(ax, x_vals, minibatches):
    """Plot extracted epoch data for a single model. See _plot_model_logs."""
    plotting.plot_with_confidence(
        ax, x_vals, minibatches, plotting.colors(3), label="minibatches"
    )
    ax.set_ylabel("minibatches", color=plotting.colors(3))
    ax.tick_params(axis="y", labelcolor=plotting.colors(3))
    ax.set_ylim(bottom=0)


def _plot_model_logs(loss_ax, epoch_ax, steps_ax, logs):
    """Plot losses / total training weight for a single model."""
    reps_with_gaps = plotting.pad_into_lists(
        utils.group_by(logs, lambda r: r["repetition"]).values(),
        lambda r: r["generation"],
    )
    by_repetition = [
        plotting.carry_back_first(plotting.carry_previous_over_none(rep))
        for rep in reps_with_gaps
    ]
    train_loss = [[r["loss_at_best"] for r in rep] for rep in by_repetition]
    valid_loss = [[r["valid_loss_at_best"] for r in rep] for rep in by_repetition]
    epoch_counters = [[r["epoch_counter"] for r in rep] for rep in by_repetition]
    best_valid_epoch = [[r["best_valid_epoch"] for r in rep] for rep in by_repetition]
    total_weight = [[r.get("train_total_weight") for r in rep] for rep in by_repetition]
    minibatches = [[r["batch_counter"] for r in rep] for rep in by_repetition]
    x_vals = range(1, len(train_loss[0]) + 1)

    assert len(set([len(l) for l in train_loss] + [len(l) for l in valid_loss])) == 1
    _plot_losses(loss_ax, x_vals, train_loss, valid_loss)

    _plot_epochs(epoch_ax, x_vals, epoch_counters, best_valid_epoch)

    _plot_total_weight(steps_ax, x_vals, total_weight)
    _plot_minibatches(steps_ax.twinx(), x_vals, minibatches)


def plot_train_loss(save_path, title_suffix, events):
    train_logs = [r for r in events if r["event"] == "distill_end"]
    model_logs = utils.group_by(
        train_logs, lambda r: r.get("distill_net")
    )  # we may have multiple models e.g. from two_value_func
    num_models = len(model_logs)
    fig, axss = plt.subplots(
        3, num_models, figsize=[10 * num_models, 20], squeeze=False
    )
    axss = axss.T

    for axs, (model_name, logs) in zip(axss, model_logs.items()):
        _plot_model_logs(*axs, logs)
        title = "Training summary {}".format(title_suffix.lstrip())
        axs[0].set_title(title if model_name is None else f"{title} {model_name}")
        axs[-1].set_xlabel("Generations", fontsize=16)

    fig.tight_layout()
    plt.savefig(save_path)


def plot_train_loss_from_config(config, events):
    plot_train_loss(
        plotting.format_figure_filename(config, "train_summary.png"),
        plotting.config_suffix(config),
        events,
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "run_id",
        type=str,
        help="a run ID (e.g., 2019_01_06_13_15_48_13172) or path to a config.json file",
    )
    args = parser.parse_args()

    config, logs = experiment_result.load_config_events(args.run_id)
    plot_train_loss_from_config(config, logs)


if __name__ == "__main__":
    main()

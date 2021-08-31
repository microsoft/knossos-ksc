# fmt: off
import os
import numpy as np
import argparse
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from rlo import experiment_result
from rlo import plotting
from rlo import utils

def plot_rewrite_sequences(outdir, events, linear_scale=False, plot_all=True):

    #make a new directory for the rewrite sequence plots
    if not os.path.isdir(outdir):
        os.mkdir(outdir)
    event = [e for e in events if 'eval_expr' in e and e['event']=='rollout_end' and "generation" in e]
    events_by_generation = utils.group_by(event, lambda r: r["generation"])

    # for later: might want to limit the number of generations, ok for now
    for generation, events in events_by_generation.items():
        events_by_expr = utils.group_by(events, lambda r: r["eval_expr"])
        n_expressions = len(events_by_expr)
        assert n_expressions > 0

        # for each generation create a separate plot containing all expressions
        fig, axs = plt.subplots(n_expressions, figsize=[15, 6 * (n_expressions)])
        if n_expressions == 1:
            axs = [axs]
        fig.tight_layout(rect=(0.05, 0, 1, 1))

        for ax, (expr, ev) in zip(axs, events_by_expr.items()):
            by_rep = utils.group_by(ev, lambda r: r["repetition"])
            best_ep_costs = [
                [cost for (_action, cost) in utils.single_elem(by_rep[k])["best_ep"][1]]
                for k in sorted(by_rep.keys())
            ]

            if not plot_all:
                best_ep_costs = np.array(plotting.apply_padding_by_last(best_ep_costs))
                num_steps = np.arange(best_ep_costs.shape[1])
                col = plotting.random_color_from_string(expr)
                plotting.plot_with_confidence(ax, num_steps, best_ep_costs, col, label=expr, probs=[20, 50, 80], axis=0)
            else:
                # plot rewrite sequences for all repetitions without padding
                for i, best_ep in enumerate(best_ep_costs):
                    col = plotting.random_color_from_string(i)
                    ax.plot(np.arange(len(best_ep)), best_ep, color=col, label=expr + " at rep " + str(i))

            if linear_scale:
                ax.set_yscale("linear")
            else:
                ax.set_yscale("log")
            ax.set_ylabel("cost", fontsize=8)
            ax.set_xlabel("steps", fontsize=8)
            ax.legend(*ax.get_legend_handles_labels(), loc="upper right")

        plt.savefig(outdir + "/cost_per_steps_generation_" + str(generation) + ".png")
        plt.title("Cost at each rewrite step")
        plt.close()

def plot_rewrite_sequences_from_config(config, events, linear_scale=False, plot_all=True):
    plot_rewrite_sequences(
        plotting.format_figure_filename(config, "rewrite_sequences"),
        events, linear_scale, plot_all)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("run_id", type=str, help="(1) a run ID (e.g., 2019_01_06_13_15_48_13172), (2) path to a config.json file, or (3) path to a directory containing /rep/events.json files")
    parser.add_argument("--linear_scale", default=False, action="store_true")
    parser.add_argument("--plot_summary", dest="plot_all", action="store_false")
    parser.set_defaults(plot_all=True)
    args = parser.parse_args()

    config, logs = experiment_result.load_config_events(args.run_id)
    if config is not None:
        plot_rewrite_sequences_from_config(config, logs, args.linear_scale, args.plot_all)
    else:
        plot_rewrite_sequences(os.path.join(args.run_id, "rewrite_sequences"),
                               logs,
                               args.linear_scale,
                               args.plot_all)
if __name__ == "__main__": main()

# fmt: off
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

def blas():
    from blas_result import cost_sequence, annotations
    matplotlib.rcParams.update({'font.size': 15})
    fig = plt.figure(figsize=(5, 11))
    ax = fig.add_axes([0.2, 0.92, 0.75, 0.025])
    ts = np.arange(len(cost_sequence), dtype=np.float64)
    ax.semilogy(ts, cost_sequence, linewidth=6)
    ax.set_ylim([7070100, 7200000])
    ax.set_xlabel("Steps", fontsize=16)
    ax.set_ylabel("Cost", fontsize=16)
    ax.set_yticks([7e+6, 7.2e+6])
    ax.set_yticklabels([])
    ax.spines['top'].set_visible(False)
    ax.yaxis.set_label_coords(-0.17, 2.0)
    ax1 = fig.add_axes([0.2, 0.955, 0.75, 0.045])
    ax1.semilogy(ts, cost_sequence, linewidth=6)
    ax1.set_ylim([7200000, max(cost_sequence)])
    ax1.set_xticklabels([])
    ax1.spines['bottom'].set_visible(False)
    for i, (t, cost, text) in enumerate(annotations):
        ax.annotate(
            text,
            xy=(t, cost_sequence[t]),
            xytext=(10, 320 - 310 * i),
            textcoords="figure points",
            arrowprops={"width": 2, "headwidth": 10},
            bbox={"boxstyle": "round", "facecolor": "white"},
            fontsize=11,
        )
        ax.annotate(
            "cost={}".format(cost),
            xy=(t, cost_sequence[t]),
            xytext=(10, 320 - 310 * i),
            textcoords="figure points",
            weight="bold",
            fontsize=11,
        )
    # ax1.annotate("$\\approx$", (-0.4, 0), xycoords="axes points")
    plt.savefig("cost_sequence_blas.pdf")


def mnist():
    from mnist_result import cost_sequence, annotations
    matplotlib.rcParams.update({'font.size': 16})
    fig = plt.figure(figsize=(15, 9))
    ax = fig.add_axes([0.1, 0.7, 0.8, 0.28])
    ts = np.arange(len(cost_sequence), dtype=np.float64)
    ax.semilogy(ts, cost_sequence, linewidth=10)
    ax.set_xlabel("Steps", fontsize=16)
    ax.set_ylabel("Cost function", fontsize=16)
    for i, (t, cost, text) in enumerate(annotations):
        ax.annotate(
            text,
            xy=(t, cost_sequence[t]),
            xytext=(10 + 350 * i, 10),
            textcoords="figure points",
            arrowprops={"width": 2, "headwidth": 10},
            bbox={"boxstyle": "round", "facecolor": "white"},
            fontsize=9,
        )
        ax.annotate(
            "cost={}".format(cost),
            xy=(t, cost_sequence[t]),
            xytext=(10 + 350 * i, 10),
            textcoords="figure points",
            weight="bold",
            fontsize=11,
        )
    plt.savefig("cost_sequence_mnist.pdf")

if __name__ == "__main__":
    blas()
    # mnist()

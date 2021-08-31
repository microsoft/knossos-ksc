# fmt: off
import matplotlib
matplotlib.use("Agg")
from matplotlib.patches import Polygon
import matplotlib.pyplot as plt
import numpy as np
import os
import random

from rlo import utils
from rlo import git_utils


def format_figure_filename(config, appendix):
    return os.path.join(config["result_save_path"], appendix)

def combine_legend_handles_labels(*axes):
    """ Given one or more axes, return a tuple of (lines, labels) where each label appears only once. """
    line_by_label = {}
    for ax in axes:
        lines, labels = ax.get_legend_handles_labels()
        assert len(lines) == len(labels)
        line_by_label.update(dict(zip(labels, lines)))
    labels, lines = zip(*line_by_label.items())
    return lines, labels

def plot_with_confidence(ax, x_values, y_values, col, conf_alpha=0.5, probs=[10, 50, 90], axis=0, linewidth=2, **kwargs):
    if len(probs)<2 or len(probs)>3:
        raise ValueError("Don't know what to do with {} probs - should be 2 for interval around mean or 3 to plot middle".format(len(probs)))
    y_values = np.array(y_values)
    print("number of samples: {}".format(y_values.shape[axis]))
    perc = np.nanpercentile(y_values, probs, axis=axis)
    N = len(x_values)
    verts = [(x_values[i], perc[0, i]) for i in range(N)] + [(x_values[i], perc[-1, i]) for i in range(N-1, -1, -1)]
    poly = Polygon(verts, facecolor=col, alpha=conf_alpha)
    ax.add_patch(poly)
    y_center = np.mean(y_values, axis=axis) if len(probs)==2 else perc[1, :]
    ax.plot(x_values, y_center, color=col, linewidth=linewidth, **kwargs)

def colors(i):
    colors_array = plt.rcParams['axes.prop_cycle'].by_key()['color']
    return colors_array[i % len(colors_array)]

def random_color_from_string(str):
    """ Given a string, return an RGB color string.
        The same string will always give the same result but otherwise is random. """
    rng = random.Random(str)
    cs  =    ['#d98668', '#d97400', '#bfab5c', '#aee66e', '#9bf2be', '#1b9ca6', '#0088ff', '#0000a6', '#771ba6', '#b3398a', '#e54973', '#e56e6e',
              '#ffbca3', '#a6651b', '#f1ff29', '#8cb372', '#6aa682', '#a3f9ff', '#93bfe6', '#3535a6', '#c874f2', '#bf7aa8', '#e693a9', '#a65050',
              '#b38372', '#f2b774', '#b5bf1f', '#33bf00', '#23d984', '#7abbbf', '#0066ff', '#6a6aa6', '#9456b3', '#a60058', '#a60016', '#ffa3a3',
              '#ff6600', '#a68a6a', '#eaf274', '#8cff7a', '#8bd9bf', '#41b0cc', '#4179cc', '#6256b3', '#bf25e6', '#e5258c', '#a66a72', '#bf1a00',
              '#b24700', '#e59900', '#c7cc83', '#a3ffa3', '#00f2c2', '#00a2f2', '#a3bcff', '#5825e6', '#f9a3ff', '#ffa3d4', '#ff6952', '#ff9752',
              '#b2811d', '#77b300', '#39b349', '#35a68f', '#006fa6', '#7283b3', '#8e6ee6', '#b300a7', '#cc628c', '#a64435', '#b27b56', '#ffd37a',
              '#c5ff52', '#00ff44', '#00e6d6', '#4ebbf2', '#7a8cff', '#8045d9', '#ff52f3', '#cc0036', '#d95323', '#e6b493', '#e5b800', '#88ff00',
              '#50a667', '#00e2f2', '#5694b3', '#0000b3', '#a083cc', '#e66ece', '#a61b40', '#ff0000']
    return rng.choice(cs)

def config_suffix(config):
    assert ("run_id" in config) != ("experiment_ind" in config) # Support legacy configs
    return ' [{}{}, {}]'.format(
        config.get("run_id", config.get("experiment_ind")),
        config.get("extra_scenario_params", ""),
        config.get("gitlog", git_utils.get_git_revision_short_hash()))  # Note, gitlog is of when experiment run, not when plot.

def pad_into_lists(list_of_iterables, index_func, blank_elem=None):
    """ Given a list of iterables, return a list of lists, making a list from each iterable by
            - positioning each element at index given by applying index_func
            - putting blank_elem at any index for which there is no element
            - padding all lists to the length of the longest (with blank_elem) """
    list_of_dicts = [utils.group_by(elems, index_func) for elems in list_of_iterables]
    max_key = max([k for d in list_of_dicts for k in d.keys()])
    return [ [(utils.single_elem(d[k]) if k in d else blank_elem) for k in range(max_key + 1)]
        for d in list_of_dicts]

def carry_previous_over_none(lst):
    """ Given a list, returns a copy with any None's replaced by the most-recent non-None element.
        None's at the start of the list are left intact. """
    res = []
    prev = None
    for item in lst:
        if item is not None:
            prev=item
        res.append(prev)
    return res

def apply_padding_by_last(list_of_lists):
    """ The same as applying pad_into_lists followed by carry_previous_over_none
        but takes a list of lists instead of events
    Args:
        lists_of_lists: list of lists with possibly different lengths
    Returns:
        lists of lists padded to the same length by the last element in each list
    """
    padded_lists = pad_into_lists(
        [enumerate(vals) for vals in list_of_lists],
        lambda x: x[0]
    )
    return [
        # remove the index
        [e[1] if e is not None else e for e in carry_previous_over_none(padded)]
        for padded in padded_lists
    ]


def join(lst1, lst2, index_func):
    def combine_dicts(d1, d2):
        assert all([d1[k] == d2[k] for k in d1.keys() & d2.keys() - {"event"}])
        return {k : d1.get(k, d2.get(k)) for k in d1.keys() | d2.keys()}

    d1 = utils.group_by(lst1, index_func)
    d2 = utils.group_by(lst2, index_func)
    assert d1.keys() == d2.keys()
    return {k : combine_dicts(utils.single_elem(d1[k]), utils.single_elem(d2[k])) for k in d1.keys()}

def carry_back_first(lst):
    # Replace any initial None's with the first element
    idx = next(i for i in range(len(lst)) if lst[i] is not None)
    return (lst[idx:idx+1] * idx) + lst[idx:]

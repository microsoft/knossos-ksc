"""Toy problem for investigating activations / model architectures for monotonic fns."""
from absl import app, flags
import functools
import shutil
import os
from typing import Callable, Optional, Sequence, Tuple, Union

from mpl_toolkits.mplot3d import Axes3D  # pylint: disable=unused-import

import matplotlib.pyplot as plt
import numpy as np
import tensorflow as tf

flags.DEFINE_boolean(
    "datavis", help="visualize training data / surface only", default=False
)
flags.DEFINE_string("save_dir", help="save directory", default="/tmp/monotonic-plots")
flags.DEFINE_string("tb_dir", help="tensorboard direcotry", default="")
flags.DEFINE_bool(
    "sort_x",
    help="sort x values so number of time values increases with x",
    default=False,
)
flags.DEFINE_float("p", help="power of distribution", default=0.0)
flags.DEFINE_bool("rollout", help="use rollout distribution", default=False)
flags.DEFINE_integer("nx", help="number of x values", default=32)
flags.DEFINE_integer("tm", help="max t value", default=32)
flags.DEFINE_boolean(
    "positive_final_weights",
    help="initialize weights of final weights matrix to be positive",
    default=False,
)
flags.DEFINE_string(
    "hidden_activation", default="relu", help="activation used on all hidden layers"
)

FLAGS = flags.FLAGS


Activation = Optional[Union[Callable, str]]

######## DATA ########


def f(x, t):
    """function to learn."""
    return tf.square(tf.cast(t, tf.float32) / FLAGS.tm) * (tf.math.sin(5 * x) + 1)


def get_trange(dtype=tf.int64):
    """[[0, 1, ..., FLAGS.tm-1], shape [1, FLAGS.tm]."""
    return tf.expand_dims(tf.range(FLAGS.tm, dtype=dtype), axis=0)


def get_xt_grid(x: tf.Tensor) -> Tuple[tf.Tensor, tf.Tensor]:
    """
    Args:
        x: [nx] float

    Returns:
        X, T: each of shape [nx, tm]
    """
    T = get_trange()
    t = tf.tile(T, (x.shape[0], 1))
    x = tf.tile(tf.expand_dims(x, axis=1), (1, FLAGS.tm))
    return x, t


def get_data(
    x: tf.Tensor, t: tf.Tensor, multi_head: bool, full_time: bool = True, noise=0.05
):
    """Get (x, labels, weights) if multi_head and full_time else ((x, t), labels)."""
    T = get_trange()
    Tf = tf.cast(T, tf.float32)
    labels = f(tf.expand_dims(x, axis=1), Tf)
    labels = labels + tf.random.stateless_normal(
        shape=labels.shape, dtype=tf.float32, stddev=noise, seed=(52, 13)
    )
    mask = T < tf.expand_dims(t, axis=1)

    if multi_head and full_time:
        weights = tf.cast(mask, tf.float32)
        return x, labels, weights
    else:
        x, t = get_xt_grid(x)
        x = tf.boolean_mask(x, mask)
        t = tf.boolean_mask(t, mask)
        labels = tf.boolean_mask(labels, mask)
        t.shape.assert_has_rank(1)
        x.shape.assert_has_rank(1)
        return (x, t), labels


########### NETWORKS ###########


def softplus(x, alpha=1):
    return tf.nn.softplus(x * alpha) / alpha


def mlp(
    x: tf.Tensor,
    output_dim: int,
    units=(32, 32, 32),
    final_activation: Activation = None,
) -> tf.Tensor:
    activation = FLAGS.hidden_activation
    x.shape.assert_has_rank(2)
    for u in units:
        x = tf.keras.layers.Dense(u, activation=activation)(x)
    layer = tf.keras.layers.Dense(output_dim)
    if FLAGS.positive_final_weights:
        layer.build(x.shape)
        layer.kernel.assign(tf.abs(layer.kernel))
    x = layer(x)
    x = tf.keras.activations.get(final_activation)(x)
    return x


def multi_outputs(
    x: tf.Tensor,
    num_heads: int,
    final_activation: Activation = None,
    use_cumsum: bool = False,
    normalized_cumsum: bool = False,
    **mlp_kwargs,
) -> tf.Tensor:
    """outputs of multi-mlp."""
    out = mlp(x, output_dim=num_heads, final_activation=final_activation, **mlp_kwargs)
    if use_cumsum:
        out = tf.math.cumsum(out, axis=-1)
        if normalized_cumsum:
            out = out * (2.0 / tf.cast(tf.shape(out)[-1], tf.float32))
    return out


def multi_standard_model(**kwargs) -> tf.keras.Model:
    """multi-model trained with standard (non-full-time) batching."""
    x = tf.keras.Input(shape=(), dtype=tf.float32)
    t = tf.keras.Input(shape=(), dtype=tf.int64)
    col = multi_outputs(tf.expand_dims(x, axis=-1), FLAGS.tm, **kwargs)
    out = tf.expand_dims(tf.gather(col, t, batch_dims=1), axis=-1)

    return tf.keras.Model((x, t), out)


def multi_full_time_model(**kwargs) -> tf.keras.Model:
    """multi-model trained with full-time batching."""
    x = tf.keras.Input(shape=(), dtype=tf.float32)
    return tf.keras.Model(
        x, multi_outputs(tf.expand_dims(x, axis=-1), FLAGS.tm, **kwargs)
    )


def single_model(**mlp_kwargs) -> tf.keras.Model:
    """model which maps (x, t) -> scalar output."""
    x = tf.keras.Input(shape=(), dtype=tf.float32)
    t = tf.keras.Input(shape=(), dtype=tf.int64)
    inp = (x, t)
    out = mlp(
        tf.stack((x, tf.cast(t, tf.float32) / FLAGS.tm), axis=-1),
        output_dim=1,
        **mlp_kwargs,
    )
    return tf.keras.Model(inp, out)


class SquaredError(tf.keras.losses.Loss):
    """loss like MSE, but sum rather than mean."""

    def __init__(self, name="squared_error"):
        super().__init__(name=name)

    def __call__(self, y_true, y_pred, sample_weight=None):
        err = tf.math.squared_difference(y_true, y_pred)
        if sample_weight is not None:
            err = err * sample_weight
        return tf.reduce_sum(err)


def get_model(
    multi_head: bool = False, full_time: bool = True, **kwargs
) -> tf.keras.Model:
    """Get compiled keras model."""
    tf.random.set_seed(123)
    if multi_head:
        if full_time:
            model = multi_full_time_model(**kwargs)
        else:
            model = multi_standard_model(**kwargs)
    else:
        model = single_model(**kwargs)
    optimizer = tf.keras.optimizers.Adam()
    model.compile(
        optimizer=optimizer,
        loss=SquaredError(),
        metrics=[tf.keras.metrics.MeanSquaredError()],
    )
    return model


def fit_model(
    model: tf.keras.Model,
    dataset: tf.data.Dataset,
    epochs: int,
    steps_per_epoch: int,
    tb_dir: Optional[str] = None,
):
    examples_per_epoch = tf.data.experimental.cardinality(dataset).numpy()
    batch_size = examples_per_epoch // steps_per_epoch
    dataset = dataset.shuffle(examples_per_epoch).batch(batch_size)
    callbacks = []
    if tb_dir:
        callbacks.append(tf.keras.callbacks.TensorBoard(tb_dir, write_graph=False))
    return model.fit(dataset, epochs=epochs, callbacks=callbacks, verbose=False)


def fit(
    x,
    t,
    epochs: int,
    steps_per_epoch: int,
    tb_dir: Optional[str] = None,
    multi_head: bool = False,
    full_time: bool = True,
    **kwargs,
):
    model = get_model(multi_head=multi_head, full_time=full_time, **kwargs)
    dataset = tf.data.Dataset.from_tensor_slices(
        get_data(x, t, multi_head=multi_head, full_time=full_time)
    )
    fit_model(model, dataset, epochs, steps_per_epoch, tb_dir)
    return model


def get_preds(
    x,
    t,
    x_pred: Sequence[float],
    multi_head: bool = False,
    full_time: bool = True,
    **kwargs,
):
    """
    Args:
        x: [nx] float32 x-coordinates of training data columns
        t: [nx] int64 number of t values for each x value
        x_pred: [npx] float32 x-coordinates of returns predictions
        multi_head: hyperparam
        full_time: hyperparam
        **kwargs: used in model fit

    Returns:
        [npx, FLAGS.tm] float32 predictions
    """
    model = fit(x, t, multi_head=multi_head, full_time=full_time, **kwargs)
    if multi_head and full_time:
        return model(x_pred).numpy()
    else:
        x_pred, t = get_xt_grid(x_pred)
        x_pred = tf.reshape(x_pred, (-1,))
        t = tf.reshape(t, (-1,))
        out = model((x_pred, t)).numpy()
        return out.reshape((-1, FLAGS.tm))


########### Visualization ###########


class PltPlotter:
    """Creates slice plots."""

    def __init__(
        self,
        name: str,
        x_pred: Sequence[float],
        t: np.ndarray,
        labels: np.ndarray,
        nrows: int = 1,
        figsize=(16, 10),
        ylims=None,
    ):
        assert len(labels) % nrows == 0
        ncols = len(labels) // nrows
        self._fig, self._axes = plt.subplots(nrows, ncols, figsize=figsize)
        self._axes = self._axes.reshape((-1,))
        self._t = t
        plt.suptitle(name)
        assert len(self._axes) == len(x_pred)
        for (xp, ax) in zip(x_pred, self._axes):
            plt.sca(ax)
            if ylims is not None:
                plt.ylim(*ylims)
            plt.title("x = {:.1f}".format(xp))
            plt.xlabel("t")
        for datum, ax in zip(labels, self._axes):
            ax.plot(datum, label="labels", linestyle="dashed", color="k")
        self._name = name

    def add(self, data: np.ndarray, label: str, **scatter_kwargs):
        if len(data) != len(self._axes):
            raise ValueError(
                "data and axes must have same length but "
                f"{len(data)} != {len(self._axes)}"
            )
        for datum, ax in zip(data, self._axes):
            ax.plot(self._t, datum, label=label)

    def finalize(self):
        plt.figure(self._fig.number)
        plt.figlegend(*self._axes[-1].get_legend_handles_labels(), loc="upper left")

    def save(self, save_dir: str):
        self._fig.savefig(f"{save_dir}/slices_{self._name}.png")


class TensorBoardPlotter:
    """Plot results in tensorboard, which may be preferable for filtering."""

    def __init__(self, root_dir: str, x_pred: Sequence[float], labels: np.ndarray):
        self._x_pred = x_pred
        self._root_dir = root_dir
        self._root_dir = root_dir
        self.add(labels, "labels")

    def add(self, data: np.ndarray, label: str):
        log_dir = os.path.join(self._root_dir, label)
        with tf.summary.create_file_writer(log_dir).as_default():
            for datum, xp in zip(data, self._x_pred):
                for step, val in enumerate(datum):
                    tf.summary.scalar(f"x={xp}", val, step=step)


def plot_surface_and_points(x_grid, t_grid, x_points, t_points, labels_points):
    """
    Args:
        x_grid, t_grid: used for wireframe plot
        x_points, t_points, label_points: (x, y, z) used for scatter
    """
    T = get_trange()
    X, T_ = tf.meshgrid(x_grid, t_grid, indexing="ij")
    fx = f(X, T_)

    fig = plt.figure()

    ax = fig.gca(projection="3d")
    ax.plot_wireframe(X, T, fx, alpha=0.2, color="gray")  # pylint:disable=no-member
    ax.scatter(x_points, t_points, labels_points, c=labels_points)
    plt.show()


def plot_predictions(x_grid, predictions, save_dir: str, name: str):
    """
    Args:
        x_grid: [ngx] float32 x coordinates of predictions
        predictions: [ngx, FLAGS.tm] float32 model predictions
        save_dir: directory to save
        name: used in filename
    """
    T = get_trange()
    Tf = tf.cast(T, tf.float32)
    X, T_ = tf.meshgrid(x_grid, Tf, indexing="ij")
    fx = f(X, T_)

    fig = plt.figure()
    ax = fig.gca(projection="3d")
    ax.plot_wireframe(X, T, fx, alpha=0.2, color="gray")
    ax.plot_surface(X, T, predictions, cmap="coolwarm", vmin=0, vmax=2)
    ax.set_zlim(0, 2)
    plt.title(name)
    plt.savefig(f"{save_dir}/surf_{name}.png")


########### Putting it all together ###########


def fit_and_plot_all(
    x, t, x_grid, plot_indices, tb_dir: str = "", save_dir="monotonic-plots"
):
    """
    Args:
        x: training data, [nx] float32
        t: largest value of t for each x, [nx] int64
        x_grid: [ngx] float32 x-ordinates of grid for plotting
        plot_indices: indices into x_grid to plot slices.
        tb_dir: tensorboard directory. If "" or None, no logging is done.
        save_dir: directory to save plots.
    """
    if not os.path.isdir(save_dir):
        os.makedirs(save_dir)
    do_tb_plots = bool(tb_dir)
    nx = x.shape[0]

    labels_grid = f(*get_xt_grid(x_grid)).numpy()
    x_plot = x_grid.numpy()[plot_indices]
    labels_plot = labels_grid[plot_indices]

    uniform_kwargs = dict(x_pred=x_grid, epochs=32, steps_per_epoch=nx // 2, x=x, t=t)
    names_and_activations = (
        ("none", None),
        ("relu", tf.nn.relu),
        ("softplus-1", functools.partial(softplus, alpha=1)),
        ("softplus-5", functools.partial(softplus, alpha=5)),
        ("softplus-10", functools.partial(softplus, alpha=10)),
        ("softplus-100", functools.partial(softplus, alpha=100)),
    )

    name_and_kwargs = (
        ("single", dict(multi_head=False)),
        ("multi", dict(multi_head=True, full_time=False)),
        ("multi-ft", dict(multi_head=True)),
        ("multi-cumsum", dict(multi_head=True, full_time=False, use_cumsum=True)),
        ("multi-ft-cumsum", dict(multi_head=True, use_cumsum=True)),
        (
            "multi-cumsum-normalized",
            dict(
                multi_head=True,
                normalized_cumsum=True,
                use_cumsum=True,
                full_time=False,
            ),
        ),
        (
            "multi-ft-cumsum-normalized",
            dict(multi_head=True, normalized_cumsum=True, use_cumsum=True),
        ),
    )

    if os.path.isdir("logs"):
        shutil.rmtree("logs")

    tb_plotter = (
        TensorBoardPlotter("logs/predictions", x_plot, labels_plot)
        if do_tb_plots
        else None
    )
    T = get_trange()

    for name0, final_activation in names_and_activations:
        plt_plotter = PltPlotter(
            name0, x_plot, tf.squeeze(T, 0).numpy(), labels_plot, nrows=2, ylims=(0, 2)
        )
        for name1, kwargs in name_and_kwargs:
            print(name0, name1, kwargs)
            preds = get_preds(
                final_activation=final_activation,
                tb_dir=(
                    os.path.join("logs/summaries", name0, name1)
                    if do_tb_plots
                    else None
                ),
                **uniform_kwargs,
                **kwargs,
            )
            plt_plotter.add(preds[plot_indices], name1)
            plot_predictions(x_grid, preds, save_dir, f"{name0}_{name1}")
            if tb_plotter is not None:
                tb_plotter.add(preds, os.path.join(name0, name1))
        plt_plotter.finalize()
        plt_plotter.save(save_dir)
    print(f"Finished training / plotting. See {save_dir} for plots")


def main(_):
    nx = FLAGS.nx
    np.random.seed(123)

    x = tf.random.stateless_uniform((nx,), seed=(521, 231), minval=-1.0, maxval=1.0)
    if FLAGS.rollout:
        t = tf.range(1, FLAGS.tm + 1, dtype=tf.int64)
    else:
        probs = np.arange(FLAGS.tm, dtype=np.float32)
        probs = probs ** FLAGS.p
        probs /= np.sum(probs)
        t = tf.constant(
            FLAGS.tm - np.random.choice(FLAGS.tm, p=probs, size=nx), dtype=tf.int64
        )
    if FLAGS.sort_x:
        x = tf.sort(x)
        t = tf.sort(t)

    if FLAGS.datavis:
        (xp, tp), labels = get_data(x, t, multi_head=True, full_time=False)
        plot_surface_and_points(
            tf.linspace(-1.0, 1.0, nx), get_trange(tf.float32), xp, tp, labels,
        )
        return

    x_grid = tf.linspace(-1.0, 1.0, 21)

    fit_and_plot_all(
        x,
        t,
        x_grid,
        plot_indices=[8, 12, 15, 17],
        save_dir=FLAGS.save_dir,
        tb_dir=FLAGS.tb_dir,
    )


if __name__ == "__main__":
    app.run(main)

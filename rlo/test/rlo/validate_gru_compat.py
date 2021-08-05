import numpy as np
import tensorflow as tf

tf.compat.v1.enable_eager_execution()
tf.compat.v1.enable_v2_tensorshape()


def transfer_weights(
    src: tf.compat.v1.nn.rnn_cell.GRUCell, dst: tf.keras.layers.GRUCell
):
    """
    Transfer weights from `src` to `dst` so `src(x) == dst(x)`.

    Weights of `src` are not changed, so `src(x)`.

    Returns a tuple of weight update ops or 3 None values if executing eagerly.
    """
    wg, bg, wc, bc = src.weights
    w, wr, b = dst.weights
    in_size = w.shape[0]

    wg0, wg1 = tf.split(wg, 2, axis=-1)
    w_t = tf.concat((wg1, wg0, wc), axis=-1)  # [in_size + out_size, 3 * out_size]

    bg0, bg1 = tf.split(bg, 2)
    return (
        w.assign(w_t[:in_size]),
        wr.assign(w_t[in_size:]),
        b.assign(tf.concat((bg1, bg0, bc), axis=-1)),
    )


def test_gru_compatibility():
    """
    Show compatibility of different GRU implementations.

    tf.nn.rn_cell.GRUCell (v1)
    equivalent to
    tf.keras.layer.GRUCell (v2)
    """
    in_size = 5
    out_size = 7

    in_shape = (None, in_size)

    v1 = tf.compat.v1.nn.rnn_cell.GRUCell(out_size, dtype=tf.float32)
    v2 = tf.keras.layers.GRUCell(
        out_size, reset_after=False, recurrent_activation="sigmoid",
    )
    v1.build(in_shape)
    v2.build(in_shape)

    for v in v1.weights:
        v.assign(tf.random.normal(v.shape))

    transfer_weights(v1, v2)

    batch_size = 3
    x = tf.random.normal((batch_size, in_size), dtype=tf.float32)
    state = tf.random.normal((batch_size, out_size), dtype=tf.float32)

    o1, _ = v1(x, state)
    o2, _ = v2(x, states=[state])

    # without atol this sometimes fails with smaller tolerances based on seed
    np.testing.assert_allclose(o1.numpy(), o2.numpy(), rtol=1e-5, atol=1e-5)


def weights_hist():
    """
    Visualize histograms of weights.

    Shows two histograms for each weight of `tf.keras.layers.GRUCell`, one with
    weights initialized as per default initializers, and one with values copied
    from the default initialization of a `tf.compat.v1.nn.rnn_cell.GRUCell`.

    See `transfer_weights` for the weight copying mechanism.
    """
    import matplotlib.pyplot as plt

    in_size = 128
    out_size = 256

    in_shape = (None, in_size)

    v1 = tf.compat.v1.nn.rnn_cell.GRUCell(out_size, dtype=tf.float32)
    v2 = tf.keras.layers.GRUCell(
        out_size, reset_after=False, recurrent_activation="sigmoid",
    )
    v1.build(in_shape)
    v2.build(in_shape)

    names = tuple(t.name for t in v2.weights)
    weights0 = tuple(t.numpy().flatten() for t in v2.weights)
    transfer_weights(v1, v2)
    weights1 = tuple(t.numpy().flatten() for t in v2.weights)
    fig, axes = plt.subplots(1, len(weights0))
    bins = 101
    del fig
    for ax, name, w0, w1 in zip(axes, names, weights0, weights1):
        ax.hist(w0, bins, alpha=0.5, label="keras")
        ax.hist(w1, bins, alpha=0.5, label="rnn_cell")
        ax.legend(loc="upper right")
        ax.set_title(name[:-2])
    plt.show()


if __name__ == "__main__":
    test_gru_compatibility()
    print("validate_gru_compatibility completed")
    weights_hist()

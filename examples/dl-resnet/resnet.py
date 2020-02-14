import ksc
from ksc.tracing.functions import core, math, nn

@ksc.trace
def dense(x, weights):
    W, b = weights
    return math.broadcast_add(math.dot(x, math.transpose(W)), b)

@ksc.trace
def conv_block(x, weights, strides):
    (conv_1_weights,
     norm_1_weights,
     conv_2_weights,
     norm_2_weights,
     conv_3_weights,
     norm_3_weights) = weights
    h = nn.conv_2d_no_bias(x, conv_1_weights, strides)
    h = nn.batch_norm_2d(h, norm_1_weights)
    h = nn.relu(h)
    h = nn.conv_2d_no_bias(h, conv_2_weights, (1, 1))
    h = nn.batch_norm_2d(h, norm_2_weights)
    h = nn.relu(h)
    h = nn.conv_2d_no_bias(h, conv_3_weights, (1, 1))
    return nn.batch_norm_2d(h, norm_3_weights)

@ksc.trace
def conv_residual_block(x, weights, strides):
    (conv_block_weights,
     shortcut_conv_weights,
     shortcut_norm_weights) = weights
    main = conv_block(x, conv_block_weights, strides)
    h = nn.conv_2d_no_bias(x, shortcut_conv_weights, strides)
    shortcut = nn.batch_norm_2d(h, shortcut_norm_weights)
    return nn.relu(core.add(main, shortcut))

@ksc.trace
def identity_residual_block(x, weights):
    main = conv_block(x, weights, (1, 1))
    return nn.relu(core.add(main, x))

@ksc.trace
def resnet(x, weights):
    (normalization_weights,
     conv_weights,
     batch_norm_weights,
     residual_blocks_weights,
     final_dense_weights) = weights
    h = nn.normalize_2d(x, normalization_weights)
    h = nn.conv_2d_no_bias(h, conv_weights, (2, 2))
    h = nn.batch_norm_2d(h, batch_norm_weights)
    h = nn.relu(h)
    h = nn.max_pool(h, (3, 3), (2, 2), padding="SAME")
    for i, blocks_weights in enumerate(residual_blocks_weights):
        for j, weights in enumerate(blocks_weights):
            if j == 0:
                strides = (1, 1) if i == 0 else (2, 2)
                h = conv_residual_block(h, weights, strides)
            else:
                h = identity_residual_block(h, weights)
    h = nn.avg_pool(h, (7, 7), (1, 1))
    h = core.flatten(h)
    h = dense(h, final_dense_weights)
    return nn.log_softmax(h)


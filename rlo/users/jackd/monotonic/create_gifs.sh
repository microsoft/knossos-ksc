#!/bin/bash
# Don't judge me - I do not bash
# who needs loops when we have ctrl + c -> ctrl + V?

convert -delay 100 \
    /tmp/monotonic-plots/surf_softplus-1_single.png \
    /tmp/monotonic-plots/surf_softplus-5_single.png \
    /tmp/monotonic-plots/surf_softplus-10_single.png \
    /tmp/monotonic-plots/surf_softplus-100_single.png \
    /tmp/monotonic-plots/surf_relu_single.png \
    /tmp/monotonic-plots/anim-surf_single.gif

convert -delay 100 \
    /tmp/monotonic-plots/surf_softplus-1_multi.png \
    /tmp/monotonic-plots/surf_softplus-5_multi.png \
    /tmp/monotonic-plots/surf_softplus-10_multi.png \
    /tmp/monotonic-plots/surf_softplus-100_multi.png \
    /tmp/monotonic-plots/surf_relu_multi.png \
    /tmp/monotonic-plots/anim-surf_multi.gif

convert -delay 100 \
    /tmp/monotonic-plots/surf_softplus-1_multi-ft.png \
    /tmp/monotonic-plots/surf_softplus-5_multi-ft.png \
    /tmp/monotonic-plots/surf_softplus-10_multi-ft.png \
    /tmp/monotonic-plots/surf_softplus-100_multi-ft.png \
    /tmp/monotonic-plots/surf_relu_multi-ft.png \
    /tmp/monotonic-plots/anim-surf_multi-ft.gif

convert -delay 100 \
    /tmp/monotonic-plots/surf_softplus-1_multi-cumsum.png \
    /tmp/monotonic-plots/surf_softplus-5_multi-cumsum.png \
    /tmp/monotonic-plots/surf_softplus-10_multi-cumsum.png \
    /tmp/monotonic-plots/surf_softplus-100_multi-cumsum.png \
    /tmp/monotonic-plots/surf_relu_multi-cumsum.png \
    /tmp/monotonic-plots/anim-surf_multi-cumsum.gif

convert -delay 100 \
    /tmp/monotonic-plots/surf_softplus-1_multi-ft-cumsum.png \
    /tmp/monotonic-plots/surf_softplus-5_multi-ft-cumsum.png \
    /tmp/monotonic-plots/surf_softplus-10_multi-ft-cumsum.png \
    /tmp/monotonic-plots/surf_softplus-100_multi-ft-cumsum.png \
    /tmp/monotonic-plots/surf_relu_multi-ft-cumsum.png \
    /tmp/monotonic-plots/anim-surf_multi-ft-cumsum.gif

convert -delay 100 \
    /tmp/monotonic-plots/surf_softplus-1_multi-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_softplus-5_multi-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_softplus-10_multi-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_softplus-100_multi-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_relu_multi-cumsum-normalized.png \
    /tmp/monotonic-plots/anim-surf_multi-cumsum-normalized.gif

convert -delay 100 \
    /tmp/monotonic-plots/surf_softplus-1_multi-ft-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_softplus-5_multi-ft-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_softplus-10_multi-ft-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_softplus-100_multi-ft-cumsum-normalized.png \
    /tmp/monotonic-plots/surf_relu_multi-ft-cumsum-normalized.png \
    /tmp/monotonic-plots/anim-surf_multi-ft-cumsum-normalized.gif


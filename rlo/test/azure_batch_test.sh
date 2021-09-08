#!/bin/bash
set -e
set -x
echo 'azure_batch_test.sh: start'
mkdir -p results

# Pytorch is nondeterministic on GPU, so don't include it here.
# bash ./test/determinism_test.sh --tensorflow --output_keep_prob 1.0 --graph_state_keep_prob 1.0
# bash ./test/gpu_test.sh
# bash ./test/ray_test.sh

echo 'azure_batch_test.sh: done'

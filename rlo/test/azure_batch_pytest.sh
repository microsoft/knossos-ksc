#!/bin/bash
set -e
set -x
echo 'azure_batch_pytest.sh: start'
mkdir -p results

# pytest test/rlo/ --quick --durations=5

echo 'azure_batch_pytest.sh: done'

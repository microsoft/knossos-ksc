#!/bin/bash
exec 2>&1 # interleave stdout and stderr
set -e
set -x

echo azure_batch_test start
tmpfile=`mktemp`

# Pytorch is nondeterministic on GPU, so don't include it here.
#bash ./test/determinism_test.sh --tensorflow --output_keep_prob 1.0 --graph_state_keep_prob 1.0
#bash ./test/gpu_test.sh

sh ./test/find-new-files.sh mark $tmpfile

bash ./test/ray_test.sh

echo "azure_batch_test.sh: New files:"
sh ./test/find-new-files.sh diff $tmpfile
echo "azure_batch_test.sh: New files done"

# Many of these tests also run in QuickTest and the pytest portion of BuildAndTest. However, those run on CPU.
# we only really care about the medium tests (which are also executed by BuildAndTest but on CPU).
#pytest test/rlo/ --durations=5


# next: remove lines here, does that break build?

# if not, proceed with reapplying fixes to az*ps1

# then split out into tasks, one per line,
#   name this script to match the yml name, 

# All goodies to be found on awf/tidy-ci-logs

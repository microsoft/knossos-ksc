#!/bin/bash
set -e
set -x

# Arguments that should give exactly repeatable results. Turn off dropout for determinism.
# TODO #19613 once pytorch + ray works, run this without the --tensorflow flag
REPEAT_ARGS="shuffled_probabilistic_eval --force_gpu --gpu_memory_fraction=0.49 --output_keep_prob=1.0 --graph_state_keep_prob=1.0 --tensorflow"

python src/train_over_expressions.py $REPEAT_ARGS --run_id=shuf1 --num_generations 4

# Test that Ray works, using two local workers on the same GPU. 
python src/ray_main.py $REPEAT_ARGS --run_id shuf2 --workers_per_gpu=2 --num_generations 2

# Check everything is as we would expect from a 2-generation run, before resuming for more
EXPECTED_NUM_GEN2_EVENTS=$(bzcat outputs/Run_shuf1/*/events.json.bz2 | grep 'generation": 2' | wc -l)
# bunzip2 here both moves the logs from the first half-run out of the way so they are not overwritten when we resume,
# and also means we get a test later that plotting reads both compressed and uncompressed events.json's.
bunzip2 outputs/Run_shuf2/*/events{,_1}.json.bz2
if [ $EXPECTED_NUM_GEN2_EVENTS != $(grep 'generation": 2' outputs/Run_shuf2/*/events.json | wc -l) ]; then
  echo there were additional events in generation 2 before resumption
  exit 1
fi
if grep restored outputs/Run_shuf2/*/events.json; then
  echo First run claims to have restored?
  exit 1
fi
if grep 'generation": 3' outputs/Run_shuf2/*/events.json; then
  echo First run did generation 3?
  exit 1
fi

# Run with Ray for a couple more generations. Events from this second half-run will be in .bz2's
python src/ray_main.py $REPEAT_ARGS --run_id shuf2 --workers_per_gpu=2 --num_generations 4

# Check that it claims to have restored some saved model/state
bzcat outputs/Run_shuf2/*/events.json.bz2 | grep restored | grep 'generation": 2'
# Check there were no other generation-2 events (this is a bit redundant, compare_runs below would fail if there were).
# We assume/know the python does not delete the existing events so this tells us the computation was not repeated.
if bzcat outputs/Run_shuf2/*/events.json.bz2 | grep 'generation": 2' | grep -v restored; then
  echo there were additional events in generation 2 after resumption
  exit 1
fi
# Check that ray_main.py was identical to the 4-generation run of train_over_expressions.py above
#   - ray_main.py produces start_repetition message types once for the run, rather than per repetition as train_over_expressions.py does
python src/compare_runs.py --sort shuf1 shuf2 --skip_event_types restored_model restored_state start_repetition expression_summary

# Test that Ray works, using two value functions, and produces the same results as train_over_expressions.py.
REPEAT_ARGS="fewer_simplify_rules --two_value_func=train --two_value_func_var_frac_train=0.5 --force_gpu --gpu_memory_fraction=0.49 --output_keep_prob=1.0 --graph_state_keep_prob=1.0 --tensorflow"
python src/ray_main.py $REPEAT_ARGS --run_id=fs3r --workers_per_gpu=2
python src/train_over_expressions.py $REPEAT_ARGS --run_id=fs3t
python src/compare_runs.py --sort fs3t fs3r --skip_event_types start_repetition expression_summary

if bzcat outputs/Run*/*/events.json.bz2 | grep worker_died; then
  echo "workers died during local Ray runs - shouldn't happen"
  exit 1
fi

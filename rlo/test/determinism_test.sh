#!/bin/bash
set -e
set -x

# Arguments that should give exactly repeatable results. Turn off dropout for determinism.
REPEAT_ARGS="shuffled_probabilistic_eval --seed_all_reps 1 $@"
python src/train_over_expressions.py ${REPEAT_ARGS} --num_generations=4 --run_id=foo --save_all_models 

# Check that evaluating saved models produces the same result.
python src/evaluate_saved_models.py foo
python src/compare_runs.py --vs_eval_events foo

# Check that stopping and resuming training gives same result as doing all generations in a single run.
python src/train_over_expressions.py ${REPEAT_ARGS} --run_id=bar --num_generations=2
python src/train_over_expressions.py ${REPEAT_ARGS} --run_id=bar --num_generations=4

python src/compare_runs.py foo bar --sort --skip_event_types restored_model restored_state start_repetition expression_summary

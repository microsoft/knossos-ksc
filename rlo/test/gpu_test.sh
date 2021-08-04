#!/bin/bash
set -e
set -x

# Tests to run on a GPU machine, excluding those that involve Ray.

# Test we can run without crashing with a few different options
python src/train_over_expressions.py fewer_simplify_rules --seed_all_reps=1 --force_gpu

# Check that made some graphs
ls outputs/Run_fewer_simplify_rules*/*.png

# Test a bunch more options
python src/train_over_expressions.py fewer_simplify_rules --seed_all_reps=1 --force_gpu --v2 --verbose --cost_per_step=0.1 --one_hot_embedding

# BLAS is a .kso scenario so includes the parser, save_rlo_to_ksc, and others
python src/train_over_expressions.py blas --num_repetitions=1 --num_generations=1 --max_gnn_train=100 --max_gnn_eval=50 --max_epochs=1

# Test we can run two models in parallel.
# However, we have only one GPU, and each worker in the process uses a different GPU, so we can't use --num_parallel
# Instead run in the background using bash job control.
python src/train_over_expressions.py shuffled_probabilistic_eval --force_gpu --gpu_memory_fraction=0.49 --num_generations=4 --save_all_models  &
fs1_pid=$!

# And run in parallel with foreground:
python src/train_over_expressions.py fewer_simplify_rules --sparse_gnn --gpu_memory_fraction=0.49 --force_gpu --seed_all_reps=1
# wait here will exit with the exitcode of the waited process, i.e. the previous python ... &
wait $fs1_pid

# Test we can train and search with different time_budgets
# TODO move this test to pytest?
python src/train_over_expressions.py fewer_simplify_rules --simulation_depth_train=8 --simulation_depth_eval=16

# Test we can train and search with different expression sets
python src/train_over_expressions.py binding_simplify_rollout_gelu --train_search=beam --eval_search=beam --max_gnn_train=15 --max_gnn_eval=15 --max_epochs=5 --num_repetitions=1 --num_generations=1

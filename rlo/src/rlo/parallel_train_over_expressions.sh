set +x
python train_over_expressions.py --cost_normalization=none "$@" &
pid_none=$!
python train_over_expressions.py --cost_normalization=log "$@" &
pid_log=$!
python train_over_expressions.py --cost_normalization=logspeedup "$@" &
pid_ls=$!
python train_over_expressions.py --cost_normalization=prop "$@"
wait $pid_ls
wait $pid_log
wait $pid_none

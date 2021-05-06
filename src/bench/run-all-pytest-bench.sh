# TODO: this should be a makefile.  fred.csv: fred.py etc
# BENCH=src/bench/run-bench.py

#PYTHONPATH=examples/dl-activations pytest src/bench/ --modulename=relu3 --benchmarkname=vrelu3 --benchmark-autosave
PYTHONPATH=examples/dl-capsule pytest src/bench/ --modulename=sqrl --benchmarkname=sqrl --benchmark-autosave
# TODO: this should be a makefile.  fred.csv: fred.py etc
# BENCH=src/bench/run-bench.py

#pytest src/bench/ --benchmark-sort=name --benchmark-group-by=group,func --modulepath=examples/dl-activations/relu3 --benchmarkname=vrelu3 --benchmark-autosave
pytest src/bench/ --benchmark-sort=name --benchmark-group-by=group,func --modulepath=examples/dl-capsule/sqrl --benchmarkname=sqrl --benchmark-autosave
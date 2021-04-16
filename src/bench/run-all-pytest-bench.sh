# TODO: this should be a makefile.  fred.csv: fred.py etc
# BENCH=src/bench/run-bench.py

#PYTHONPATH=examples/dl-activations python $BENCH relu3 vrelu3 
PYTHONPATH=examples/dl-capsule pytest src/bench/

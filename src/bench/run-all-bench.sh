# TODO: this should be a makefile.  fred.csv: fred.py etc
BENCH=src/bench/run-bench.py

#python $BENCH examples/dl-activations/relu3 vrelu3
python $BENCH examples/dl-capsule/sqrl sqrl

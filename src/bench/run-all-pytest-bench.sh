# TODO: this should be a makefile.  fred.csv: fred.py etc
BENCH="pytest src/bench/ --benchmark-name=short --benchmark-max-time=5.0 --benchmark-sort=name --benchmark-group-by=group,func --benchmark-autosave --benchmark-columns=median,iqr,outliers,mean,stddev,min,max,iterations,rounds"

# $BENCH --modulepath=examples/dl-activations/relu3 --benchmarkname=vrelu3
$BENCH --modulepath=examples/dl-capsule/sqrl --benchmarkname=sqrl
$BENCH --modulepath=examples/dl-activations/gelu --benchmarkname=vgelu

# TODO: this should be a makefile.  fred.csv: fred.py etc
BENCH="pytest src/bench/ --benchmark-name=short --benchmark-sort=name --benchmark-group-by=group,func --benchmark-autosave --benchmark-columns=median,mean,stddev,min,max,iqr,outliers,iterations,rounds"

# $BENCH --modulepath=examples/dl-activations/relu3 --benchmarkname=vrelu3
$BENCH --modulepath=examples/dl-capsule/sqrl --benchmarkname=sqrl
$BENCH --modulepath=examples/dl-activations/gelu --benchmarkname=vgelu

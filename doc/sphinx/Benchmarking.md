# Benchmarking


We benchmark the Python parts of Knossos with https://pytest-benchmark.readthedocs.io


## Nightly benchmark runs

We have a GitHub Action that runs nightly [here](../.github/workflows/benchmark.yml)

## Getting existing results

Results are stored on Azure storage.

https://knossosbenchmark.blob.core.windows.net/benchmarks

A way to get these is to install azure-cli

https://docs.microsoft.com/en-us/cli/azure/install-azure-cli-linux?pivots=apt


```
az login

az account set --subscription knossos

az storage copy --source https://knossosbenchmark.blob.core.windows.net/benchmarks/* --destination .benchmarks --recursive
```

## Making comparisons 


We have a script to do plots against commits (note: this currently includes all current benchmarks)

```
python src/bench/comparison_plot.py
```

then load build/sqrl.html

### pytest-benchmark comparisons

pytest-benchmark also has built in comparisons

```
py.test-benchmark compare --group-by=group,func --sort=name
```

it can also draw histograms
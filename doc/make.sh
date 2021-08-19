PYTHONPATH=../examples/dl-activations:../src/python \
 python -c 'import relu3 ; relu3.plot_relu3("../build/doc/relu3-plot.png");'
sphinx-build sphinx ../build/doc

PYTHONPATH=../examples/dl-activations:../src/python  python -c 'import relu3 ; relu3.plot_relu3("sphinx/relu3-plot.png");'
sphinx-build sphinx ../build/doc

export PYTHONPATH=../examples/dl-activations:../src/python
python -c 'import relu3 ; relu3.plot_relu3("../build/doc/relu3-plot.png");'
# make relu3 plots
python -c 'import relu3 ; relu3.plot_relu3_via_knossos("../build/doc/relu3-grad-plot.png");'

# make relu3.cpp
relu_cpp=../build/torch_extensions/KscStub_VSelem___main___relu3/ks-main.cpp
sh ../etc/prettify-cpp.sh < $relu_cpp | egrep -B 1 -A 20 '^ty.relu3 relu3\(' | tee sphinx/examples/relu3.cpp

sphinx-build sphinx ../build/doc

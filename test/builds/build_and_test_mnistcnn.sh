set -e

KNOSSOS=$1
PYBIND11=$2
MODULE_NAME=mnistcnncpp
CPP_FILE=$KNOSSOS/test/ksc/mnistcnnpy.cpp

. $KNOSSOS/test/builds/build_pybind11_module.sh

build_pybind11_module $KNOSSOS $PYBIND11 $MODULE_NAME $CPP_FILE

echo Installing JAX...

python3 -m pip install jax==0.1.57 jaxlib==0.1.37

KSCPY=$KNOSSOS/src/python
PYTHONPATH=$OBJ:$KSCPY python3 -m ksc.mnist.test

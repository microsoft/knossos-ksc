set -e

KNOSSOS=$1
PYBIND11=$2
MODULE_NAME=adbench_lstm
CPP_FILE=$KNOSSOS/test/ksc/adbench-lstmpy.cpp

. $KNOSSOS/test/builds/build_pybind11_module.sh

build_pybind11_module $KNOSSOS $PYBIND11 $MODULE_NAME $CPP_FILE

KSCPY=$KNOSSOS/src/python
PYTHONPATH=$OBJ:$KSCPY python3 -m ksc.adbench_lstm.test

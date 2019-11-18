# There's a lot of duplication between this and
# build_and_test_adbench_lstm.sh, but we will follow the Rule of Three
#
#     https://en.wikipedia.org/wiki/Rule_of_three_(computer_programming)

set -e

KNOSSOS=$1
PYBIND11=$2

RUNTIME=$KNOSSOS/src/runtime
OBJ=$KNOSSOS/obj/test/ksc
PYBIND11_INCLUDE=$PYBIND11/include

PYTHON3_CONFIG_EXTENSION_SUFFIX=$(python3-config --extension-suffix)

MODULE_NAME=gmm
MODULE_FILE="$OBJ/$MODULE_NAME$PYTHON3_CONFIG_EXTENSION_SUFFIX"

echo Compiling...

g++-7 -fmax-errors=5 \
      -fdiagnostics-color=always \
      -Wall \
      -Wno-unused \
      -Wno-maybe-uninitialized \
      -I$RUNTIME \
      -I$OBJ \
      -I$PYBIND11_INCLUDE \
      $(PYTHONPATH=$PYBIND11 python3 -m pybind11 --includes) \
      -O3 \
      -std=c++17 \
      -shared \
      -fPIC \
      -o $MODULE_FILE \
      -DMNISTCNNCPP_MODULE_NAME=$MODULE_NAME \
      $KNOSSOS/test/ksc/gmmpy.cpp

KSCPY=$KNOSSOS/src/python
PYTHONPATH=$OBJ:$KSCPY python3 -m ksc.gmm.test

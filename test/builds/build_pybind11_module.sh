build_pybind11_module() {
    KNOSSOS=$1
    PYBIND11=$2
    MODULE_NAME=$3
    CPP_FILE=$4

    RUNTIME=$KNOSSOS/src/runtime
    OBJ=$KNOSSOS/obj/test/ksc
    PYBIND11_INCLUDE=$PYBIND11/include

    PYTHON3_CONFIG_EXTENSION_SUFFIX=$(python3-config --extension-suffix)

    MODULE_FILE="$OBJ/$MODULE_NAME$PYTHON3_CONFIG_EXTENSION_SUFFIX"

    echo Compiling...

    # Following the recipe from
    #
    #     https://pybind11.readthedocs.io/en/master/basics.html
    g++-7 -fmax-errors=5 \
	  -fdiagnostics-color=always \
	  -Wall \
	  -Wno-unused \
	  -Wno-maybe-uninitialized \
	  -I$RUNTIME \
	  -I$OBJ \
	  -I$PYBIND11_INCLUDE \
		-DKS_NO_MAIN \
	  $(PYTHONPATH=$PYBIND11 python3 -m pybind11 --includes) \
	  -O3 \
	  -std=c++17 \
	  -shared \
	  -fPIC \
	  -o $MODULE_FILE \
	  -DPYTHON_MODULE_NAME=$MODULE_NAME \
	  $CPP_FILE
}

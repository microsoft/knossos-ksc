set -e



echo Installing dependencies...

export PYTHONPATH="./src/python"

TMPDIR="obj/test"
mkdir -p $TMPDIR

echo Translating Knossos to Python
python3 -m ksc.translate examples/dl-resnet/resnet_v2.ks --backend jax_input_last |\
  grep -v 'No CUDA runtime is found' > $TMPDIR/resnet_v2.py

PYTHONPATH="$PYTHONPATH":$TMPDIR 

echo Running test of Resnet50 written in Knossos
python ./test/builds/test_resnet50.py --model resnet_v2

echo Running test of Resnet50 written in Python
python ./test/builds/test_resnet50.py --model resnet_py

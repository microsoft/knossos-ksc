set -e



echo Installing dependencies...

python3 -m pip install -U Pillow jax==0.1.57 jaxlib==0.1.37

echo Installing ksc...
cd ./src/python
python3 -m pip install --editable .
cd ../..

echo Translating Knossos to Python
python3 -m ksc.translate examples/dl-resnet/resnet_v2.ks --backend jax_input_last > resnet_v2.py

echo Running test of Resnet50 written in Knossos
PYTHONPATH=. python ./test/builds/test_resnet50.py --model resnet_v2

echo Running test of Resnet50 written in Python
PYTHONPATH=. python ./test/builds/test_resnet50.py --model resnet_py


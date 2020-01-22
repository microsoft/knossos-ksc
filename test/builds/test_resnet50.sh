set -e



echo Installing dependencies...

python3 -m pip install -U Pillow jax==0.1.57 jaxlib==0.1.37

echo Installing ksc...
cd ./src/python
python3 -m pip install .
cd ../..

echo Translating Knossos to Python
python3 -m ksc.translate examples/dl-resnet/resnet_v2.ks --backend jax > resnet_v2.py

echo Running test...
PYTHONPATH=. python ./test/builds/test_resnet50.py

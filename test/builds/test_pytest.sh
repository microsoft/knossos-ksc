set -e

echo Installing dependencies...
python3 -m pip install -r src/python/requirements.txt
python3 -m pip install pytest numpy torch==1.8.1+cu111 jax==0.1.57 jaxlib==0.1.37 -f https://download.pytorch.org/whl/torch_stable.html

echo Installing ksc...
cd ./src/python
python3 -m pip install --editable .
cd ../..

echo Running pytest
python3 -m pytest test/python

echo Running pytest on ts2k
python3 -m pytest test/ts2k

echo Running pytest using cpp backend
python3 -m pytest test/python/test_tracing_core.py --backend cpp

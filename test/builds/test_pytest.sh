set -e

echo Installing dependencies...
python3 -m pip install pytest numpy torch jax==0.1.57 jaxlib==0.1.37

echo Installing ksc...
cd ./src/python
python3 -m pip install --editable .
cd ../..

echo Running pytest
python3 -m pytest test/python

echo Installing TS2KS...
cd ./src/ts2k
python3 -m pip install --editable .
cd ../..

echo Running pytest on ts2k
python3 -m pytest test/ts2k

echo Running pytest using cpp backend
python3 -m pytest test/python/test_tracing_core.py --backend cpp

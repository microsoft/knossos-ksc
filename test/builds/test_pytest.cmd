set -e

echo Set Pip to specific version...
python -m pip install --upgrade pip==21.0.1

echo Installing dependencies...
python -m pip install -r src/python/requirements.txt
python -m pip install pytest numpy torch==1.8.0+cu111 -f https://download.pytorch.org/whl/torch_stable.html

echo Installing ksc...
cd ./src/python
python -m pip install --editable .
cd ../..

echo Running pytest
python -m pytest test/python

echo Installing TS2KS...
cd ./src/ts2k
python -m pip install --editable .
cd ../..

echo Running pytest on ts2k
python -m pytest test/ts2k

echo Running pytest using cpp backend
python -m pytest test/python/test_tracing_core.py --backend cpp

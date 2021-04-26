echo Installing dependencies...
python3 -m pip install -r src/bench/requirements.txt

echo Installing ksc...
cd ./src/python
python3 -m pip install --editable .
cd ../..

echo Installing TS2KS...
cd ./src/ts2k
python3 -m pip install --editable .
cd ../..

sh ./src/bench/run-all-pytest-bench.sh
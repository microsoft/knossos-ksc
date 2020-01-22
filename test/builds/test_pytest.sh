set -e

echo Installing dependencies...

python3 -m pip install pytest numpy

echo Installing ksc...
cd ./src/python
python3 -m pip install .
cd ../..

echo Running pytest
python3 -m pytest test/python

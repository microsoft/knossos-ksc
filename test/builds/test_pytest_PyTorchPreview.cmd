set -e

echo Set Pip to specific version...
python -m pip install --force-reinstall pip==20.3

REM New resolver with PyTorch version notation https://pip.pypa.io/en/latest/user_guide/#changes-to-the-pip-dependency-resolver-in-20-3-2020
REM Too big, throw memory exception https://stackoverflow.com/a/31526029/35544

REM currently have backend issues on PyTorch 1.8.0 https://github.com/microsoft/knossos-ksc/issues/659 so trying nightly
echo Installing dependencies...
python -m pip install -r src/python/requirements.txt
python -m pip install --use-deprecated=legacy-resolver --no-cache-dir pytest numpy torch==1.8.0+cu111 -f https://download.pytorch.org/whl/nightly/cu111/torch_nightly.html

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



REM echo Running pytest on ts2k
python -m pytest test/ts2k

REM echo Running pytest using cpp backend
REM python -m pytest test/python/test_tracing_core.py --backend cpp

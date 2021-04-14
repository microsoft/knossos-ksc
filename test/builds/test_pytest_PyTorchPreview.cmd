set -e

set PreferredToolArchitecture=x64

IF defined GITHUB_ACTIONS (
  ECHO on GitHub Actions, activating vcbuild environment
  call %~dp0vcbuild.cmd -arch=x64 -host_arch=x64
)

where cl.exe

echo Set Pip to specific version...
python -m pip install --force-reinstall pip==20.3 || exit /b

REM New resolver with PyTorch version notation https://pip.pypa.io/en/latest/user_guide/#changes-to-the-pip-dependency-resolver-in-20-3-2020
REM Too big, throw memory exception https://stackoverflow.com/a/31526029/35544

REM currently have backend issues on PyTorch 1.8.0 https://github.com/microsoft/knossos-ksc/issues/659 so trying nightly
echo Installing dependencies...
python -m pip install -r src/python/requirements.txt || exit /b
python -m pip install --pre --use-deprecated=legacy-resolver --no-cache-dir pytest numpy torch -f https://download.pytorch.org/whl/nightly/cu111/torch_nightly.html || exit /b

echo Installing ksc...
cd ./src/python
python -m pip install --editable . || exit /b
cd ../..

echo Running pytest
pytest test/python || exit /b

echo Installing TS2KS...
cd ./src/ts2k
python -m pip install --editable . || exit /b
cd ../..

REM we only run a single test for now, multiple tests run out of heap see #679
REM pytest test\ts2k\test_ts2k.py -k test_cat

REM echo Running pytest on ts2k
REM pytest test/ts2k || exit /b

REM this runs the tests one by one avoiding the memory issues
powershell.exe -file "%~dp0multiprocess_ts2k_tests.ps1" || exit /b
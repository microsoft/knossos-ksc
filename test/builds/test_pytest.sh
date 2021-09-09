set -e

echo Installing dependencies...
python3 -m pip install \
  -r src/python/requirements.txt \
  -r src/python/requirements-unix.txt \
  -f https://download.pytorch.org/whl/torch_stable.html

echo Running pytest '(+ doctest)'
python3 -m pytest test/python --doctest-modules src/python/ksc/path.py

echo Running pytest on ts2k
python3 -m pytest test/ts2k

echo Running pytest using cpp backend
python3 -m pytest test/python/test_tracing_core.py --backend cpp

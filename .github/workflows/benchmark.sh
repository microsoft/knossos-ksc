set -e

echo Installing dependencies...
python3 -m pip install \
    -r src/python/requirements.txt \
    -r src/bench/requirements.txt \
    -f https://download.pytorch.org/whl/torch_stable.html

sh -e ./src/bench/run-all-pytest-bench.sh

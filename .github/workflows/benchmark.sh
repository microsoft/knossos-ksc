echo Installing dependencies...
python3 -m pip install -r src/bench/requirements.txt -f https://download.pytorch.org/whl/torch_stable.html

sh ./src/bench/run-all-pytest-bench.sh

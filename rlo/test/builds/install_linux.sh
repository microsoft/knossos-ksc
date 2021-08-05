#!/bin/bash
# This script installs packages required by Knossos using only pip, without conda.
# If you are happy to use conda, then instead of using this to install dependencies, you can do 
# `conda env create -f test/builds/conda-env.yaml -n knossos`
set -e

echo ----- upgrade pip -----
python -m pip install --upgrade pip
echo ----- pip install -----
pip install -r test/builds/cpu_requirements.txt
python test/builds/print_requirements.py | pip install -r /dev/stdin -r test/builds/ci-requirements.txt
echo ----- ffmpeg install -----
sudo apt-get update && sudo apt-get install -y ffmpeg

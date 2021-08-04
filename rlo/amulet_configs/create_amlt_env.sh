#!/bin/bash
# This script creates a conda environment with amlt installed, following instructions from https://phillytools.azurewebsites.net/master/setup.html

# Replace ~/miniconda3 in the line below with wherever you have installed anaconda.
source ~/miniconda3/etc/profile.d/conda.sh  
conda create -n amlt python=3.8
conda activate amlt
python -mpip install -U pip
pip install -U amlt --extra-index-url https://msrpypi.azurewebsites.net/stable/7e404de797f4e1eeca406c1739b00867 --extra-index-url https://azuremlsdktestpypi.azureedge.net/K8s-Compute/D58E86006C65



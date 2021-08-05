# mcr.microsoft.com/azureml/openmpi... are images that AzureML starts from 
# when you specify environment for AzureML using a conda environment file.
# Use the CUDA 10.1 image for compatibility with Tensorflow 2.2, which is not tested with CUDA 10.2
FROM mcr.microsoft.com/azureml/openmpi3.1.2-cuda10.1-cudnn7-ubuntu18.04
RUN conda --version  # Check conda is installed
ADD conda-env.yaml /
RUN apt-get update && apt-get install -y curl
RUN curl -sL https://aka.ms/InstallAzureCLIDeb | bash

# Avoid running out of space by setting PIP_NO_CACHE_DIR=1
ENV PIP_NO_CACHE_DIR=1

# This makes it possible to `conda activate` in next line.
RUN ln -s /opt/miniconda/etc/profile.d/conda.sh /etc/profile.d/conda.sh

# Also clear the conda cache after installing everything
RUN bash --login -c 'conda activate && conda env update --file conda-env.yaml && conda clean -ay'

# Check python, pytorch, and tensorflow are actually installed
RUN python --version
RUN python -c "import torch; print('torch version: ', torch.__version__)"
RUN python -c "import tensorflow; print('tensorflow version:  ', tensorflow.__version__)"

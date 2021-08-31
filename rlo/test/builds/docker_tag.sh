#!/bin/sh
# echo the version tag for the current Docker image.
# While we have Azure Batch, this must match what's in azure_batch_common.ps1.
echo "$(git rev-parse --short=10 $(git hash-object test/builds/Docker/Dockerfile))$(git rev-parse --short=10 $(git hash-object test/builds/conda-env.yaml))"
#!/bin/bash
# Run from root directory of repo: ./scripts/amlt_runner.sh ....
set -x
export DOCKER_TAG=$(./test/builds/docker_tag.sh)
export GITLOG=$(python3 ./scripts/print_git_revision.py)
export AMLT_ACR_ACCESS_TOKEN=$(./test/builds/docker_password.sh | tr -d '\r')
amlt run "$@"
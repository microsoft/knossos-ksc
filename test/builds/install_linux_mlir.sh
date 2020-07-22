#!/bin/bash

set -e

echo ----- apt-get update -----
sudo apt-get update
echo ----- ninja-build -----
sudo apt-get install ninja-build

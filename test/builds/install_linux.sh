#!/bin/bash

set -e

echo ----- add gcc7 repo -----
sudo add-apt-repository ppa:jonathonf/gcc-7.1
echo ----- add ghc repo -----
sudo add-apt-repository ppa:hvr/ghc -y
echo ----- apt-get update -----
sudo apt-get update
echo ----- apt-get install gcc ghc -----
sudo apt-get install gcc-7 g++-7 ghc-8.6.3
echo ----- perftools -----
sudo apt-get install libgoogle-perftools-dev google-perftools
echo ----- cabal update -----
cabal update
echo ----- cabal install -----
cabal install --with-compiler /opt/ghc/8.6.3/bin/ghc-8.6.3 hspec parsec mtl hashable

#!/bin/bash

set -e

git submodule init
git submodule update

cabal sandbox init
cabal sandbox add-source ./deps/api-builder
cabal sandbox add-source ./deps/vk-api

cabal install --dependencies-only --ghcjs
cabal configure --ghcjs
cabal build

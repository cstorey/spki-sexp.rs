#!/bin/bash

source "$(dirname $0)/env.sh"

if [ ! -x $MULTIRUST_DIR/bin/cargo ]; then
  (
    cd /tmp
    git clone  --recursive https://github.com/brson/multirust.git multirust
    cd multirust
    git submodule update --init
    ./build.sh 
    ./install.sh --prefix=$MULTIRUST_DIR
  )
fi

if [ -f RUST-VERSION ]; then
  read version < RUST-VERSION
  multirust override "$version"
fi

rustc --version
cargo --version

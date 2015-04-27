#!/bin/bash

source "$(dirname $0)/env.sh"

if [ ! -x $MULTIRUST_DIR/bin/multirust ]; then
  (
    cd /tmp
    git clone  --recursive https://github.com/brson/multirust.git multirust
    cd multirust
    git submodule update --init
    ./build.sh 
    ./install.sh --prefix=$MULTIRUST_DIR
  )
fi

multirust default nightly

if [ ! -z "$RUST_VERSION" ]; then
  multirust override "$RUST_VERSION"
elif [ -f RUST-VERSION ]; then
  read version < RUST-VERSION
  multirust override "$version"
fi

rustc --version
cargo --version

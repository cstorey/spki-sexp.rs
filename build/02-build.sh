#!/bin/bash -xe
source "$(dirname $0)/env.sh"
cargo build --verbose
cargo test --verbose

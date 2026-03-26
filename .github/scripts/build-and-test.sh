#!/usr/bin/env bash
set -euo pipefail

build_dir="${BUILD_DIR:-build}"
build_type="${BUILD_TYPE:-Release}"

cmake -S . -B "${build_dir}" -DCMAKE_BUILD_TYPE="${build_type}"
cmake --build "${build_dir}" --config "${build_type}" --parallel
ctest --test-dir "${build_dir}" -C "${build_type}" --output-on-failure

$ErrorActionPreference = "Stop"

$buildDir = if ($env:BUILD_DIR) { $env:BUILD_DIR } else { "build" }
$buildType = if ($env:BUILD_TYPE) { $env:BUILD_TYPE } else { "Release" }

cmake -S . -B $buildDir -DCMAKE_BUILD_TYPE=$buildType
cmake --build $buildDir --config $buildType --parallel
ctest --test-dir $buildDir -C $buildType --output-on-failure

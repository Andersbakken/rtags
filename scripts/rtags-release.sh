#!/bin/bash -e

mkdir build
cd build
for generator in TGZ TBZ2; do
    cmake .. -DRTAGS_ENABLE_DEV_OPTIONS=1 $CMAKE_ARGS -DCPACK_GENERATOR=$generator
    make package_source
done

exit 0

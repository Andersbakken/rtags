#!/bin/bash -e

mkdir build
cd build
for generator in TGZ TBZ2; do
    cmake .. -DRTAGS_ENABLE_DEV_OPTIONS=1 $CMAKE_ARGS -DCPACK_GENERATOR=$generator
    if [ -e "Makefile" ]; then
        make package_source
    else
        ninja package_source
    fi
done

exit 0

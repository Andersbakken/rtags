#!/bin/bash

echo "configure $PWD" ### /home/abakken/dev/rtags/v8/src/v8

cd v8-7.9.198/
export PATH=$PWD/depot_tools:$PATH
# gclient sync


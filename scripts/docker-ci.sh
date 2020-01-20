#!/bin/bash -e
# docker-ci.sh ---
# Copyright (c) Christian Schwarzgruber <c.schwarzgruber.cs@gmail.com>
# Author: Christian Schwarzgruber
# Created: Thu Jan 23 20:46:13 2020 (+0100)
# Description:
#

git clone --depth 1 --recursive https://github.com/Andersbakken/rtags
mkdir rtags/build
cd rtags/build
cmake .. -DWITH_TESTS=1
make -j9
ctest --verbose

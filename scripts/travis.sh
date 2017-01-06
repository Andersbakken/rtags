#!/bin/bash -e
# travis.sh --- setup and compile script for travis
#
# Copyright (c) Christian Schwarzgruber <c.schwarzgruber.cs@gmail.com>
# Author: Christian Schwarzgruber
# Created: Tue May 10 22:28:28 2016 (+0200)
#
# This file is part of RTags (http://rtags.net).
#
# RTags is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# RTags is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RTags.  If not, see <http://www.gnu.org/licenses/>. */
#
# Description: The following variables can be changed from the build matrix:
#  - ASAN        (default value is "1")
#  - LUA_VERSION (default value is "5.3.2")
#  - LUA_DISABLE (default value is "", set it to anything to disable lua
#                 extension for that matrix)
ASAN=${ASAN:-"1"}
declare -a CMAKE_PARAMS=("-DASAN=$ASAN"
                         "-DCMAKE_CXX_COMPILER=$CXX-$COMPILER_VERSION"
                         "-DCMAKE_C_COMPILER=$CC-$COMPILER_VERSION"
                         "-DRTAGS_NO_ELISP_BYTECOMPILE=1")

if [ $TRAVIS_OS_NAME = osx ]; then
    TRAVIS_OS_NAME=mac$TRAVIS_OS_NAME
fi

LUA_DISABLE=${LUA_DISABLE:-""}
if [ ! $LUA_DISABLE ]; then
    CMAKE_PARAMS+=("-DLUA_ENABLED=1")
    echo "Running build with Lua extension."
else
    echo "Running build without Lua extension."
fi # end ! $LUA_DISABLE

echo "Using compilers $CXX-$COMPILER_VERSION and $CC-$COMPILER_VERSION."
mkdir build && pushd build > /dev/null
cmake "${CMAKE_PARAMS[@]}" ..
make VERBOSE=1 -j2

if [ -z "$SKIP_TESTS" ]; then
    PATH=$(pwd)/bin:$PATH
    popd > /dev/null
    nosetests --nocapture
else
    echo "Skipping tests for this platform."
fi

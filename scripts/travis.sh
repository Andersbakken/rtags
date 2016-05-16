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
    LUA_VERSION=${LUA_VERSION:-"5.3.2"}
    LUA_INSTALL_DIR=$(readlink -f $TRAVIS_BUILD_DIR/..)/lua/install

    echo "Building Lua $LUA_VERSION."
    pushd .. > /dev/null
    git clone --depth 1 --branch $LUA_VERSION https://github.com/lua/lua.git
    cd lua
    make $TRAVIS_OS_NAME local
    popd > /dev/null

    # The build infrustructure cmake version is 2.8.7, and does not provide a
    # FindLua.cmake file, only FindLua50.cmake and FindLua51.cmake. Hence, cmake
    # will fail to detect Lua.
    #
    # SOLUTION:
    # Misuse FindLua51.cmake; replace `find_package(Lua 5.3)` with
    # `find_package(Lua51 5.3)`, and all `LUA_FOUND` with `LUA51_FOUND`.
    echo "Applying LUA src/CMakeLists.txt hack (misuse FindLua51.cmake)."
    sed -i 's/\(find_package(Lua\).*/\151)/;s/LUA_FOUND/LUA51_FOUND/g' src/CMakeLists.txt
    CMAKE_PARAMS+=("-DLUA_INCLUDE_DIR=$LUA_INSTALL_DIR/include"
                   "-DLUA_LIBRARY=$LUA_INSTALL_DIR/lib/liblua.a")
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

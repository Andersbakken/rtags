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
declare -a CMAKE_PARAMS=("-DCMAKE_CXX_COMPILER=$CXX$COMPILER_VERSION"
                         "-DBUILD_TESTING=1"
                         "-DCMAKE_C_COMPILER=$CC$COMPILER_VERSION")

if [ "$ASAN" ]; then
    CMAKE_PARAMS+=("-DASAN=address,undefined")
fi

LUA_DISABLE=${LUA_DISABLE:-""}
if [ ! $LUA_DISABLE ]; then
    CMAKE_PARAMS+=("-DLUA_ENABLED=1")
    echo "Running build with Lua extension."
else
    echo "Running build without Lua extension."
fi # end ! $LUA_DISABLE

function build()
{
    mkdir build && pushd build > /dev/null
    emacs --version
    cmake "${CMAKE_PARAMS[@]}" .. || cat CMakeFiles/CMakeError.log
    make VERBOSE=1 -j2
}

# All arguments will be passed on to ctest
function run_tests()
{
    PATH=$(pwd)/bin:$PATH
    ctest --output-on-failure --verbose $@
}

function osx()
{
    ## Step -- Setup
    brew update
    rm /usr/local/include/c++
    brew install llvm yarn cppunit
    brew upgrade python3
    python3 -m pip install --upgrade pip
    pip3 install --user --upgrade nose PyHamcrest
    # Add nosetest bin dir to the env path var
    PATH=$PATH:/Users/travis/Library/Python/3.6/bin

    ## Step -- Build
    build

    ## Step -- Test
    run_tests -E unittests
}

function gnu_linux()
{
    ## Step -- Setup
    pip3 install --user --upgrade nose PyHamcrest

    ## Step -- Build
    build

    ## Step -- Test
    run_tests
}

if [ $TRAVIS_OS_NAME = osx ]; then
    osx
else
    gnu_linux
fi

exit 0

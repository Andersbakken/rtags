#!/bin/bash -e
# travis.sh --- setup and compile script for travis
#
# Copyright (c) Christian Schwarzgruber <c.schwarzgruber.cs@gmail.com>
# Author: Christian Schwarzgruber
# Created: Tue May 10 22:28:28 2016 (+0200)
#
# This file is part of RTags (https://github.com/Andersbakken/rtags).
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
# along with RTags.  If not, see <https://www.gnu.org/licenses/>. */
#
# Description: The following variables can be changed from the build matrix:
#  - ASAN        (default value is "1")
declare -a CMAKE_PARAMS=("-DCMAKE_CXX_COMPILER=$CXX$COMPILER_VERSION"
                         "-DCMAKE_C_COMPILER=$CC$COMPILER_VERSION"
                         "-DWITH_TESTS=1")

if [ "$ASAN" ]; then
    CMAKE_PARAMS+=("-DASAN=address,undefined")
fi

export CCACHE_DEBUG=1
function build_and_test()
{
    cmake_args=$1; shift
    mkdir build
    cd build
    cmake "$cmake_args" "${CMAKE_PARAMS[@]}" .. || cat CMakeFiles/CMakeError.log
    make VERBOSE=1 -j2
    PATH=$PATH:$(pwd)/bin
    ctest --output-on-failure --verbose "$@"
}

function add_cmake_params()
{
    for param in "$@"; do
        CMAKE_PARAMS[${#CMAKE_PARAMS[@]}]="$param"
    done
}

function osx()
{
    mkdir -p ~/.local/bin
    export LIBCLANG_LLVM_CONFIG_EXECUTABLE=$(find /usr/local/Cellar/llvm/*/bin -name llvm-config 2>/dev/null)
    # Help cmake to find openssl includes/library
    add_cmake_params "-DOPENSSL_ROOT_DIR=/usr/local/opt/openssl"

    # Note sure why the "elisptests" target is generated even though Emacs is to old (Works locally) :/
    build_and_test -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -E elisp
}

function gnu_linux()
{
    build_and_test -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
}

pip3 install --user --upgrade wheel setuptools
pip3 install --user --upgrade pytest

if [ $TRAVIS_OS_NAME = osx ]; then
    export PATH=$PATH:$(python3 -c 'import site; print(site.USER_BASE)')/bin
    osx
else
    gnu_linux
fi

exit 0

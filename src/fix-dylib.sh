#!/bin/sh

#nstall_name_tool -change "@executable_path/../lib/libclang.3.2.dylib" "@executable_path/../3rdparty/clang_install/lib/libclang.3.2.dylib" rdm

REAL=`readlink "$1"/3rdparty/clang_install/lib/libclang.dylib`
install_name_tool -change "@executable_path/../lib/`basename \"$REAL\"`" "@executable_path/../3rdparty/clang_install/lib/`basename \"$REAL\"`" $2

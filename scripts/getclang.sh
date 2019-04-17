#!/bin/bash

VERSION="$1"
[ -z "${VERSION}" ] && VERSION="3.8.1"

LLVM=llvm-${VERSION}.src
LLVM_FILE=${LLVM}.tar.xz
CLANG=cfe-${VERSION}.src
CLANG_FILE=${CLANG}.tar.xz
LIBCXX=libcxx-${VERSION}.src
LIBCXX_FILE=${LIBCXX}.tar.xz

function dnload()
{
    local FILE=$1-${VERSION}.src.tar.xz
    local URL=https://releases.llvm.org/${VERSION}/${FILE}
    curl -L -w "%{http_code}" -z ${FILE} ${URL} -o ${FILE} 2>/dev/null
}

STATUS=$(dnload llvm)
if [ "${STATUS}" == 200 ] || [ ! -d llvm ]; then
    tar xf ${LLVM_FILE}
    rm -rf llvm
    mv ${LLVM} llvm
    dnload cfe >/dev/null
    tar xf ${CLANG_FILE}
    rm -f llvm/tools/clang
    mv ${CLANG} llvm/tools/clang

    if [ "$(uname -s)" = "Darwin" ]; then
        dnload libcxx >/dev/null
        tar xf ${LIBCXX_FILE}
        mv ${LIBCXX} llvm/projects/libcxx
    fi
fi

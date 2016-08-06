#!/bin/bash

VERSION="$1"
[ -z "${VERSION}" ] && VERSION="3.8.1"

LLVM=llvm-${VERSION}.src
LLVM_FILE=${LLVM}.tar.xz
CLANG=cfe-${VERSION}.src
CLANG_FILE=${CLANG}.tar.xz
LIBCXX=libcxx-${VERSION}.src
LIBCXX_FILE=${LIBCXX}.tar.xz

STATUS=`curl -w "%{http_code}" -z ${LLVM_FILE} http://llvm.org/releases/${VERSION}/${LLVM_FILE} -o ${LLVM_FILE}`
if [ ${STATUS} == 200 ] || [ ! -d llvm ]; then
    tar xf ${LLVM_FILE}
    rm -rf llvm
    mv ${LLVM} llvm
    curl -z ${CLANG_FILE} http://llvm.org/releases/${VERSION}/${CLANG_FILE} -o ${CLANG_FILE}
    tar xf ${CLANG_FILE}
    rm -f llvm/tools/clang
    mv ${CLANG} llvm/tools/clang

    if [ `uname -s` = Darwin ]; then
        curl -z ${LIBCXX_FILE} http://llvm.org/releases/${VERSION}/${LIBCXX_FILE} -o ${LIBCXX_FILE}
        tar xf ${LIBCXX_FILE}
        mv ${LIBCXX} llvm-${VERSION}.src/projects/libcxx
    fi
fi

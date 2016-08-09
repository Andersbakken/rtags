#!/bin/bash

VERSION="$1"
[ -z "${VERSION}" ] && VERSION="3.8.1"

CHECKSUMDIR=`dirname "${BASH_SOURCE[0]}"`/../llvm-checksums/

LLVM=llvm-${VERSION}.src
LLVM_FILE=${LLVM}.tar.xz
CLANG=cfe-${VERSION}.src
CLANG_FILE=${CLANG}.tar.xz
LIBCXX=libcxx-${VERSION}.src
LIBCXX_FILE=${LIBCXX}.tar.xz

function dnload()
{
    local FILE=$1-${VERSION}.src.tar.xz
    local URL=http://llvm.org/releases/${VERSION}/${FILE}
    STATUS=`curl -w "%{http_code}" -z ${FILE} http://llvm.org/releases/${VERSION}/${FILE} -o ${FILE} 2>/dev/null`
    if [ "${STATUS}" == 200 ] || [ "${STATUS}" == 304 ]; then
        local CHECKSUM=`cat "${CHECKSUMDIR}/${FILE}.sha512"`
        local ACTUAL=`sha512sum $FILE | awk '{print $1}'`
        if [ "${CHECKSUM}" != "$ACTUAL" ]; then
            echo -e "Invalid checksum, expected:\n${CHECKSUM}\nGot:\n${ACTUAL}\n" >&2
            exit 1
        fi
    fi
    echo ${STATUS}
}

STATUS=`dnload llvm`
if [ "${STATUS}" == 200 ] || [ ! -d llvm ]; then
    tar xf ${LLVM_FILE}
    rm -rf llvm
    mv ${LLVM} llvm
    dnload cfe >/dev/null
    tar xf ${CLANG_FILE}
    rm -f llvm/tools/clang
    mv ${CLANG} llvm/tools/clang

    if [ `uname -s` = Darwin ]; then
        dnload libcxx >/dev/null
        tar xf ${LIBCXX_FILE}
        mv ${LIBCXX} llvm/projects/libcxx
    fi
fi

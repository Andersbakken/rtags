#!/bin/bash

function dnload()
{
    local URL="$1"
    local FILE=`basename $URL`
    local DASHZ=
    if [ -e "$FILE" ]; then
        DASHZ="-z ${FILE}"
    fi
    curl -L ${DASHZ} ${URL} -o ${FILE}
}

dnload "https://github.com/v8/v8/archive/7.9.198.tar.gz"
if [ ! -d v8-7.9.198 ]; then
    tar xf "7.9.198.tar.gz"
fi

if [ ! -d v8-7.9.198/depot_tools ]; then
    mkdir -p v8-7.9.198/depot_tools
    pushd v8-7.9.198/depot_tools
    dnload "https://chromium.googlesource.com/chromium/tools/depot_tools.git/+archive/e90e5fe206c98bf7ade94f6831ed6ace48c1f5fa.tar.gz"
    tar xf "e90e5fe206c98bf7ade94f6831ed6ace48c1f5fa.tar.gz"
    popd
fi

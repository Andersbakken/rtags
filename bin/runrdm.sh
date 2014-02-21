#!/bin/bash
RDM="./rdm"
if [ ! -e "$RDM" ]; then
    RDM="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/rdm"
fi
if [ ! -e "$RDM" ]; then
    echo "Can't find rdm"
    exit 1;
fi

while true; do
    "$RDM" $@
    RET=$?
    echo "Rdm exit $RET"
    if [ "$RET" -eq 123 ]; then
        echo "Updating rdm..."
        pushd "$(git rev-parse --show-toplevel)"
        git pull
        git submodule update
        if [ -f "Makefile" ]; then
            make
        elif [ -f "build.ninja" ]; then
            ninja
        else
            echo "Don't know how to build"
            exit $RET
        fi
        popd
    else
        exit $RET
    fi
done

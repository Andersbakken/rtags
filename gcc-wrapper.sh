#!/bin/bash

test -x "`which gcc-rtags-hook.sh`" && gcc-rtags-hook.sh $0 "$@" &

if [ -z "$GCC_WRAPPER_BYPASS" ]; then
    which -a "`basename $0`" | while read i; do
        resolved=`python -c "import os, sys; print os.path.realpath(\"$i\")"`
        if [ `basename $resolved` != "gcc-wrapper.sh" ]; then
            $i "$@"
            exit $?
        fi
	DIR=`dirname $i`
	PATH=`echo $PATH | sed -e "s,$DIR,,g"`
    done
else
    echo "Ignored $0 $@"
fi

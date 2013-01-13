#!/bin/bash

function followSymlink()
{
    python -c "import os, sys; print os.path.realpath(\"$1\")"
}

hook=`followSymlink $BASH_SOURCE | sed -e 's,gcc-wrapper.sh$,gcc-rtags-hook.sh,'`
test -x "$hook" && "$hook" $0 "$@" &

if [ -z "$GCC_WRAPPER_BYPASS" ]; then
    which -a "`basename $0`" | while read i; do
        resolved=`followSymlink $i`
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

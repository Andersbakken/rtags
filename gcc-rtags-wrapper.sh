#!/bin/bash

function followSymlink()
{
    python -c "import os, sys; print os.path.realpath(\"$1\")"
}

for i in `which -a "\`basename $0\`"`; do
    resolved=`followSymlink $i`
    if [ `basename $resolved` != "gcc-rtags-wrapper.sh" ]; then
        [ -z "$RTAGS_DISABLED" ] && [ -x "`which rc`" ] && rc --silent --compile "$i" "$@" &
        [ "$RTAGS_RMAKE" ] && exit 0
        "$i" "$@"
        exit $?
    fi
    # DIR=`dirname $i`
    # PATH=`echo $PATH | sed -e "s,$DIR,,g"`
done

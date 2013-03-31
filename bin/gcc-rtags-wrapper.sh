#!/bin/bash

function followSymlink()
{
    python -c "import os, sys; print os.path.realpath(\"$1\")"
}

for i in `which -a "\`basename $0\`"`; do
    resolved=`followSymlink $i`
    if [ `basename $resolved` != "gcc-rtags-wrapper.sh" ]; then
        [ -n "$RTAGS_SERVER_FILE" ] && RTAGS_ARGS="$RTAGS_ARGS -n$RTAGS_SERVER_FILE"
        [ -n "$RTAGS_PROJECT" ] && RTAGS_ARGS="$RTAGS_ARGS --with-project=$RTAGS_PROJECT"
        [ -z "$RTAGS_COMPILE_TIMEOUT" ] && RTAGS_COMPILE_TIMEOUT=3000
        rc=`which rc`
        args=`which rtags_args`
        [ -n "$args" ] && RTAGS_ARGS="$RTAGS_ARGS `$args \"$@\"`"

        if [ -z "$RTAGS_DISABLED" ] && [ -x "$rc" ]; then
            $rc --timeout=$RTAGS_COMPILE_TIMEOUT $RTAGS_ARGS --silent --compile "$i" "$@" &
            disown
        fi
        [ "$RTAGS_RMAKE" ] && exit 0
        "$i" "$@"
        exit $?
    else
        dir=`dirname $i`
        PATH=`echo $PATH | sed -e "s,$dir/*:*,,g"`
    fi
done

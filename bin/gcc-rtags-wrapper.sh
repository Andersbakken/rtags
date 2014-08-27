#!/bin/bash

function followSymlink()
{
    python -c "import os, sys; print(os.path.realpath(\"$1\"))"
}

if [ "RTAGS_GCC_WRAPPER" = "1" ]; then
    echo 1>&2 "Recursive invocation of gcc-rtags-wrapper.sh detected"
    exit 1
fi
rc=`which rc`
for i in `which -a "\`basename $0\`"`; do
    resolved=`followSymlink $i`
    filename=`basename $resolved`
    if [ "$filename" != "gcc-rtags-wrapper.sh" ] && [ -z "$PLAST" -o "$filename" != "plastc" ]; then
        [ -n "$RTAGS_SERVER_FILE" ] && RTAGS_ARGS="$RTAGS_ARGS -n$RTAGS_SERVER_FILE"
        [ -n "$RTAGS_PROJECT" ] && RTAGS_ARGS="$RTAGS_ARGS --project-root=$RTAGS_PROJECT"
        [ -z "$RTAGS_COMPILE_TIMEOUT" ] && RTAGS_COMPILE_TIMEOUT=3000

        if [ -z "$RTAGS_DISABLED" ] && [ -x "$rc" ]; then
            $rc --timeout=$RTAGS_COMPILE_TIMEOUT $RTAGS_ARGS --silent --compile "$i" "$@" &
            disown &>/dev/null # rc might be finished by now and if so disown will yell at us
        fi
        [ "$RTAGS_RMAKE" ] && exit 0

        export RTAGS_GCC_WRAPPER=1
        "$i" "$@"
        exit $?
    else
        dir=`dirname $i`
        PATH=`echo $PATH | sed -e "s,$dir/*:*,,g"`
    fi
done
exit 1 ### no compiler found?

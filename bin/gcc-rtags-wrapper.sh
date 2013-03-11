#!/bin/bash

function followSymlink()
{
    python -c "import os, sys; print os.path.realpath(\"$1\")"
}

rc=`which rc`
for i in `which -a "\`basename $0\`"`; do
    resolved=`followSymlink $i`
    if [ `basename $resolved` != "gcc-rtags-wrapper.sh" ]; then
        if [ -z "$RTAGS_DISABLED" ] && [ -x "$rc" ]; then
            if [ "$RTAGS_SERVER_FILE" ]; then
                $rc -n "$RTAGS_SERVER_FILE" --silent --compile "$i" "$@" &
            else
                $rc --silent --compile "$i" "$@" &
            fi
        fi
        [ "$RTAGS_RMAKE" ] && exit 0
        "$i" "$@"
        exit $?
    else
        dir=`dirname $i`
        PATH=`echo $PATH | perl -i -e "while (<>) {s,$dir/?:,,; print;}"`
    fi
done

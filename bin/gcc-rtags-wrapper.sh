#!/bin/bash

if [ "$RTAGS_GCC_WRAPPER" = "1" ]; then
    echo 1>&2 "Recursive invocation of gcc-rtags-wrapper.sh detected"
    exit 1
fi

pushd . >/dev/null
if pwd | grep -q /.tup/mnt/@tupjob-; then
    cd "/${PWD#*/.tup/mnt/@tupjob-*/}"
fi

rc=$(which rc)
for i in $(which -a "$(basename "$0")"); do
    filename="$i"
    max=10
    while [ $max -gt 0 -a -L "$filename" ]; do
        max=$((max - 1))
        link=$(readlink "$filename")
        if echo "$link" | grep --quiet "^/"; then
            filename="$link"
        else
            filename="$(dirname $filename)/$link"
        fi
    done

    filename=$(basename "$filename")
    if [ "$filename" != "gcc-rtags-wrapper.sh" ] && [ -z "$PLAST" -o "$filename" != "plastc" ]; then
        [ -n "$RTAGS_SERVER_FILE" ] && RTAGS_ARGS="$RTAGS_ARGS -n$RTAGS_SERVER_FILE"
        [ -n "$RTAGS_PROJECT" ] && RTAGS_ARGS="$RTAGS_ARGS --project-root=$RTAGS_PROJECT"
        [ -z "$RTAGS_COMPILE_TIMEOUT" ] && RTAGS_COMPILE_TIMEOUT=3000

        if [ -z "$RTAGS_DISABLED" ] && [ -x "$rc" ]; then
            $rc --timeout="$RTAGS_COMPILE_TIMEOUT" $RTAGS_ARGS --silent --compile "$i" "$@" &
            disown &>/dev/null # rc might be finished by now and if so disown will yell at us
        fi
        [ "$RTAGS_RMAKE" ] && exit 0

        export RTAGS_GCC_WRAPPER=1
        popd >/dev/null
        "$i" "$@"
        exit $?
    else
        PATH="$(echo "$PATH" | sed -e "s,$(dirname "$i")/*:*,,g")"
    fi
done
exit 1 ### no compiler found?

#!/bin/bash

if [ -z "$RTAGS_DISABLED" ] && [ -x "`which rc`" ]; then
    if [ -n "$RTAGS_CPP" ]; then
        rc --silent --cpp="$RTAGS_CPP" --compile "$@"
    else
        rc --silent --compile "$@"
    fi
fi



#!/bin/bash

if [ -z "$RTAGS_DISABLED" ] && [ -x "`which rc`" ]; then
    rc --silent --compile "$ICECC_CXX" "$@" &
fi

[ "$RTAGS_RMAKE" ] && exit 1
exit 0

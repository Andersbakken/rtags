#!/bin/bash

if [ -z "$RTAGS_DISABLED" ] && [ -x "`which rc`" ]; then
    rc --timeout=3000 --silent --compile "$ICECC_CXX" "$@" &
fi

[ "$RTAGS_RMAKE" ] && exit 1
exit 0

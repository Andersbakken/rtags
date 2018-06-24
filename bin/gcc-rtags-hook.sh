#!/bin/sh

if [ -z "$RTAGS_DISABLED" ] && [ -x "$(command -v rc)" ]; then
    rc --silent --compile "$ICECC_CXX" "$@" &
fi

[ "$RTAGS_RMAKE" ] && exit 1
exit 0

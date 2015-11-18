#!/bin/bash

[ -z "$RTAGS_DISABLED" ] && [ -x "$(which rc)" ] && rc --silent --compile "$@" &
[ -n "$RTAGS_RMAKE" ] && exit 0
compiler="$1"
shift
$compiler "$@"
exit $?

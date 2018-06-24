#!/bin/sh

[ -z "$RTAGS_DISABLED" ] && [ -x "$(command -v rc)" ] && rc --silent --compile "$@" &
[ "$RTAGS_RMAKE" ] && exit 1
compiler="$1"
shift
$compiler "$@"
exit $?

#!/bin/bash

[ -z "$RTAGS_DISABLED" ] && [ -x "`which rc`" ] && rc --silent --compile "$@" &
[ "$RTAGS_RMAKE" ] && exit 1
compiler="$1"
shift
$compiler "$@"
exit $?

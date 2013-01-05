#!/bin/bash

test -z "$RTAGS_DISABLED" && test -x "`which rc`" && rc --silent --compile "$@"



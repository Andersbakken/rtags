#!/bin/bash

rdm --projects-file=project  &
sleep 3

check()
{
    result=`$1 | awk -F/ '{print $NF}'`
    if [ "$result" == "$2" ]; then
        echo "passed: $1 => $result"
    else
        echo "failed: $1 => \"$result\" != \"$2\""
    fi
}

check "rc -f main.cpp,233 -N" main.cpp,83

rc -q

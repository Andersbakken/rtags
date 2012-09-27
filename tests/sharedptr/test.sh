#!/bin/bash

rdm --projects-file=project -L rdm.log --silent &
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

checkFollow()
{
    if [ -n "$2" ]; then
        check "rc -f main.cpp,$1 -N" "main.cpp,$2"
    else
        check "rc -f main.cpp,$1 -N" ""
    fi
}

checkFollow 97 22
checkFollow 106
checkFollow 121 22
checkFollow 111 22
checkFollow 126
checkFollow 173 22
checkFollow 182 22
checkFollow 184
checkFollow 193 66
checkFollow 205 66
checkFollow 197 97

rc -q

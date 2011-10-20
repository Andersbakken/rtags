#!/bin/bash

function runtest {
    cd $1
    pwd=`pwd`
    rm -rf .rtags.db
    qmake
    make > /dev/null 2> /dev/null
    rb Makefile > /dev/null 2> /dev/null

    prev=
    fail=0
    while read line; do
        if [ ! -z "$line" ]; then
            if [ -z "$prev" ]; then
                prev=$line
            else
                result=`../../rc --follow-symbol ${pwd}/${prev}`
                #echo "${prev} => ${line} and result=${result}"
                if [ "${pwd}/${line}" != "${result}" ]; then
                    result=$(echo $result | awk 'BEGIN { FS = "/"} ; { print $NF }')
                    echo "Test ${1} failed, ${prev} is ${result}, expected ${line}"
                    fail=1
                fi
                prev=
            fi
        fi
    done < expect.txt
    if [ $fail -eq 0 ]; then
        echo "Test ${1} passed"
    fi
    cd ..
}

for test in `find . -maxdepth 1 -mindepth 1 -type d`; do
    test -f $test/expect.txt && runtest $test
done

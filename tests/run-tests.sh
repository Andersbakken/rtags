#!/bin/bash

function runtest {
    cd $1
    pwd=`pwd`
    rm -rf .rtags.db
    qmake
    make > /dev/null 2> /dev/null
    rb Makefile > /dev/null 2> /dev/null

    fail=0
    passed=0
    while read expect; do
        if [ ! -z "$expect" ]; then
            #echo $expect foo
            if echo "$expect" | grep "^rc.*=>" --quiet; then
                expected=`echo $expect | sed -e 's,.*=> ,,'`
                cmd=`echo $expect | sed -e 's,^,../../,' -e 's, =>.*,,'`
                result=`$cmd`
                #echo $expected $result
                if [ "$expected" != "$result" ]; then
                    fail=`expr $fail + 1`
                    echo "Test ${1} failed, ${expect} is ${result}, expected ${expected}"
                else
                    passed=`expr $passed + 1`
                fi
            fi
        fi
    done < expect.txt
    echo "Test ${1} $fail failed, $passed passed"
    cd ..
}

for test in `find . -maxdepth 1 -mindepth 1 -type d`; do
    test -f $test/expect.txt && runtest $test
done

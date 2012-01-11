#!/bin/bash

function runtest
{
    cd $1
    pwd=`pwd`
    rm -rf .rtags.db
    qmake
    make > /dev/null 2> /dev/null
    rb Makefile > /dev/null 2> /dev/null
    lines=`wc -l expect.txt | cut -d' ' -f1`
    echo "Running test in $1 ($lines lines)"

    fail=0
    passed=0
    while read expect; do
        if [ ! -z "$expect" ]; then
            if echo "$expect" | grep "^rc.*=>" --quiet; then
                cmd=`echo $expect | sed -e 's, =>.*,,'`
                result=`eval $cmd`

                expected=`echo $expect | sed -e 's,.*=> ,,'`
                if [ "$expected" != "$result" ]; then
                    fail=`expr $fail + 1`
                    echo "Test ${1} failed"
                    echo "$expect"
                    echo "Got      [${result}]"
                    echo "Expected [${expected}]"
                else
                    passed=`expr $passed + 1`
                fi
            fi
        fi
        if [ `expr \`expr $fail + $passed\` % 5` -eq 0 ]; then
            echo "failed $fail passed $passed total $lines"
        fi
    done < expect.txt
    echo "Test ${1} $fail failed, $passed passed"
    cd ..
}

if [ -n "$1" ]; then
    while [ -n "$1" ]; do
        runtest "$1"
        shift
    done
else
    for test in `find \`dirname ${0}\` -maxdepth 1 -mindepth 1 -type d`; do
        test -f $test/expect.txt && runtest $test
    done
fi

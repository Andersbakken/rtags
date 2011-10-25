#!/bin/bash

function runtest {
    cd $1
    pwd=`pwd`
    rm -rf .rtags.db
    qmake
    make > /dev/null 2> /dev/null
    rb Makefile > /dev/null 2> /dev/null

    type=
    input=
    fail=0
    while read expect; do
        if [ ! -z "$expect" ]; then
	    if [ -z "$type" ]; then
		type=$expect
            elif [ -z "$input" ]; then
                input=$expect
            else
                result=`../../rc ${type} ${pwd}/${input}`
                #echo "${input} => ${expect} and result=${result}"
                if [ "${pwd}/${expect}" != "${result}" ]; then
                    result=$(echo $result | awk 'BEGIN { FS = "/"} ; { print $NF }')
                    echo "Test ${1} failed, ${input} is ${result}, expected ${expect}"
                    fail=1
                fi
                input=
		type=
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

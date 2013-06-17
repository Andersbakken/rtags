#!/bin/bash
function append()
{
    #echo "log(\"before $1\");" >> rtags-esprima.js
    cat $1 >> rtags-esprima.js
    #echo "log(\"after $1\");" >> rtags-esprima.js
}
rm -f rtags-esprima.js
# echo "if (typeof log === 'undefined') log = console.log; " > rtags-esprima.js
append esprima/esprima.js
append estraverse/estraverse.js
append escope/escope.js
append esrefactor/lib/esrefactor.js
append rtags.js

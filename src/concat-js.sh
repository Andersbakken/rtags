#!/bin/bash
function append()
{
    #echo "log(\"before $1\");" >> rtags-esprima.js
    cat $1 >> rtags-esprima.js
    #echo "log(\"after $1\");" >> rtags-esprima.js
}
rm -f rtags-esprima.js
# echo "if (typeof log === 'undefined') log = console.log; " > rtags-esprima.js
append $1/esprima/esprima.js
append $1/estraverse/estraverse.js
append $1/escope/escope.js
append $1/esrefactor/lib/esrefactor.js
append $1/rtags.js

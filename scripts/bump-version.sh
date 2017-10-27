#!/bin/bash

SCRIPT="$(readlink -f ${BASH_SOURCE[0]})"
DIR="$(dirname $SCRIPT)"

# echo "$SCRIPT $DIR"

MAJOR=`echo $1 | awk -F. '{print $1}'`
MINOR=`echo $1 | awk -F. '{print $2}'`

if ! echo "$1" | grep -q "^[0-9]\+\.[0-9]\+$"; then
    echo "Bad argument: \"$1\""
    echo "Usage bump-version.sh 3.22"
    exit 1
fi

SED=sed
[ -n "`which gsed`" ] && SED=gsed

$SED -i"" -e "s,^set(RTAGS_VERSION_MAJOR [0-9]\+),set(RTAGS_VERSION_MAJOR $MAJOR)," -e "s,^set(RTAGS_VERSION_MINOR [0-9]\+),set(RTAGS_VERSION_MINOR $MINOR)," "$DIR/../CMakeLists.txt"
$SED -i"" -e "s,^(defconst rtags-package-version \"[0-9]\+\.[0-9]\+\"),(defconst rtags-package-version \"${MAJOR}.${MINOR}\")," "$DIR/../src/rtags.el"
$SED -i"" -e "s,https://andersbakken\.github\.io/rtags-releases/rtags-[0-9]\+\.[0-9]\+\.tar\.,https://andersbakken.github.io/rtags-releases/rtags-$MAJOR.$MINOR.tar.,g" "$DIR/../README.org"

#!/bin/bash -e

SCRIPT="$(readlink -f ${BASH_SOURCE[0]})"
DIR="$(dirname $SCRIPT)"

MAJOR=$(echo $1 | awk -F. '{print $1}')
MINOR=$(echo $1 | awk -F. '{print $2}')

if ! echo "$1" | grep -q "^[0-9]\+\.[0-9]\+$"; then
    echo "Bad argument: \"$1\""
    echo "Usage bump-version.sh 3.22"
    exit 1
fi

JOBS=$(getconf _NPROCESSORS_ONLN)

if [ "$(uname)" == "Darwin" ]; then
    SED=$(which gsed)
else
    SED=$(which sed)
fi

if [ ! -x "$SED" ]; then
    echo "You need sed installed (and on Mac it needs to be gsed) to run ${BASH_SOURCE[0]}"
    exit 1
fi

cd $DIR/..
$SED -i""                                                                       \
     -e "s,^set(RTAGS_VERSION_MAJOR [0-9]\+),set(RTAGS_VERSION_MAJOR $MAJOR),"  \
     -e "s,^set(RTAGS_VERSION_MINOR [0-9]\+),set(RTAGS_VERSION_MINOR $MINOR),"  \
     CMakeLists.txt
$SED -i""                                                                                                                       \
     -e "s,^(defconst rtags-package-version \"[0-9]\+\.[0-9]\+\"),(defconst rtags-package-version \"${MAJOR}.${MINOR}\"),"      \
     -e "s,^;; Version: [0-9]\+\.[0-9]\+,;; Version: ${MAJOR}.${MINOR},"                                                        \
     src/rtags.el

echo "Generating manpages"
if [ ! -d build ]; then
    mkdir build
fi
cd build
cmake ..
make man -j$JOBS
cd ..

git commit -m "Bump version to ${MAJOR}.${MINOR}"       \
    CMakeLists.txt                                      \
    src/rtags.el                                        \
    man
git tag -a "v${MAJOR}.${MINOR}" -m "RTags release ${MAJOR}.${MINOR}"
git push --follow-tags

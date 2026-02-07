#!/bin/bash -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

MAJOR=$(echo $1 | awk -F. '{print $1}')
MINOR=$(echo $1 | awk -F. '{print $2}')
DATABASE=$(echo $1 | awk -F. '{print $3}')
TAG="v${MAJOR}.${MINOR}"

if git rev-parse $TAG >/dev/null 2>&1; then
  echo "$TAG exists"
  exit 1
fi

JOBS=$(getconf _NPROCESSORS_ONLN)

SED=$(command -v sed)

if [ ! -x "$SED" ]; then
    echo "You need sed installed to run ${BASH_SOURCE[0]}"
    exit 1
fi

# BSD sed (macOS) requires a separate argument after -i, GNU sed does not.
if $SED --version >/dev/null 2>&1; then
    SED_INPLACE=("$SED" -i"")
else
    SED_INPLACE=("$SED" -i "")
fi

cd $DIR/..

if ! echo "$1" | grep -qE "^[0-9]+\.[0-9]+\.[0-9]+$"; then
    echo "Bad argument: \"$1\""
    CURRENT_MAJOR=$(grep -o "^set(RTAGS_VERSION_MAJOR [0-9]*" CMakeLists.txt | awk '{print $2}')
    CURRENT_MINOR=$(grep -o "^set(RTAGS_VERSION_MINOR [0-9]*" CMakeLists.txt | awk '{print $2}')
    CURRENT_DATABASE=$(grep -o "^set(RTAGS_VERSION_DATABASE [0-9]*" CMakeLists.txt | awk '{print $2}')
    echo "Usage bump-version.sh ${CURRENT_MAJOR}.${CURRENT_MINOR}.${CURRENT_DATABASE}"
    exit 1
fi

"${SED_INPLACE[@]}" -E                                                                  \
     -e "s,^set\(RTAGS_VERSION_MAJOR [0-9]+\),set(RTAGS_VERSION_MAJOR $MAJOR),"         \
     -e "s,^set\(RTAGS_VERSION_MINOR [0-9]+\),set(RTAGS_VERSION_MINOR $MINOR),"         \
     -e "s,^set\(RTAGS_VERSION_DATABASE [0-9]+\),set(RTAGS_VERSION_DATABASE $DATABASE)," \
     CMakeLists.txt
"${SED_INPLACE[@]}" -E                                                                                                         \
     -e "s,^\(defconst rtags-package-version \"[0-9]+\.[0-9]+\"\),\(defconst rtags-package-version \"${MAJOR}.${MINOR}\"\),"   \
     -e "s,^;; Version: [0-9]+\.[0-9]+(\.[0-9]+)?,;; Version: ${MAJOR}.${MINOR}.${DATABASE},"                                  \
     src/rtags.el

echo "Generating manpages"
if [ ! -d build ]; then
    mkdir build
fi
cd build
cmake -G "Unix Makefiles" ..
make man -j$JOBS
cd ..

git commit -m "Bump version to ${MAJOR}.${MINOR}"       \
    CMakeLists.txt                                      \
    src/rtags.el                                        \
    man
git tag -a $TAG -m "RTags release ${MAJOR}.${MINOR}"

echo "You can now push the release to github"

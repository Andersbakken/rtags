#!/bin/bash

SCRIPT_PATH=`dirname "$0"`;
SCRIPT_PATH=`eval "cd \"$SCRIPT_PATH\" && pwd"`
if [ -n "$RTAGS_RELEASES_REPO" ]; then
    if [ ! -d "$RTAGS_RELEASES_REPO" ]; then
        git clone git@github.com:Andersbakken/rtags-releases.git "$RTAGS_RELEASES_REPO"
    fi
    cd "$SCRIPT_PATH/.."
    rm -f rtags.tar rtags.tar.gz
    $SCRIPT_PATH/git-archive-all $RTAGS_RELEASES_REPO/rtags.tar
    commit=`git show --oneline --no-patch`
    cd "$RTAGS_RELEASES_REPO"
    gzip --keep rtags.tar
    bzip2 rtags.tar
    git pull
    git add rtags.tar.gz rtags.tar.bz2
    git commit -m "Release for $commit"
    git push &
fi

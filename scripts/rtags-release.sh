#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    SCRIPT_PATH="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$SCRIPT_PATH/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
SCRIPT_PATH="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

cd "$SCRIPT_PATH"
branch_name="$(git symbolic-ref HEAD 2>/dev/null)" || branch_name="(unnamed branch)"     # detached HEAD
branch_name=${branch_name##refs/heads/}

RELEASES=~/Downloads/rtags-releases
if [ "$branch_name" == "master" ]; then
    commit=$(git show --oneline --no-patch)
    current=`curl --silent http://andersbakken.github.io/rtags-releases/commit | cut -d' ' -f1`
    if [ "`echo "$commit" | cut -d' ' -f1`" != "$current" ]; then
        rm -rf $RELEASES
        mkdir $RELEASES
        $SCRIPT_PATH/git-archive-all --prefix rtags/ $RELEASES/rtags.tar
        cd $RELEASES
        git init
        git checkout -b gh-pages
        gzip --keep rtags.tar
        bzip2 rtags.tar
        echo "$commit" > commit
        git add rtags.tar.gz rtags.tar.bz2 commit
        git commit -m "Release for $commit"
        git push -f git@github.com:Andersbakken/rtags-releases.git gh-pages
        rm -rf $RELEASES
    fi
fi

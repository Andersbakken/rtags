#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    SCRIPT_PATH="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$SCRIPT_PATH/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
SCRIPT_PATH="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
REPO=$SCRIPT_PATH/../

PULL=
while [ -n "$1" ]; do
    case "$1" in
        --pull|-p)
            PULL=1
            ;;
        --repo|-r)
            shift
            REPO=$1
            ;;
        *)
            echo "Unknown argument $1"
            exit 1
            ;;
    esac
    shift
done
cd "$REPO"
UNSTASH=
if [ "$PULL" ]; then
    if [ -n "`git status -s -uno 2>/dev/null`" ]; then
        git stash
        UNSTASH=1
    fi
    if ! git pull --recurse-submodules > /dev/null; then
        if [ -n "$UNSTASH" ]; then
            git stash pop
            exit 1
        fi
    fi
fi
branch_name="$(git symbolic-ref HEAD 2>/dev/null)" || branch_name="(unnamed branch)"     # detached HEAD
branch_name=${branch_name##refs/heads/}

RELEASES=~/Downloads/rtags-releases
if [ "$branch_name" == "master" ]; then
    commit=$(git show --oneline --no-patch)
    current=`curl --silent http://andersbakken.github.io/rtags-releases/commit | cut -d' ' -f1`
    if [ "`echo "$commit" | cut -d' ' -f1`" != "$current" ]; then
        rm -rf "$RELEASES"
        mkdir "$RELEASES"
        tar --exclude-vcs --transform 's,^,rtags/,' -cvzf $RELEASES/rtags.tar.gz .
        tar --exclude-vcs --transform 's,^,rtags/,' -cvzf $RELEASES/rtags.tar.bz2 .
        $SCRIPT_PATH/git-archive-all --prefix rtags/ $RELEASES/rtags.tar
        cd $RELEASES
        cp rtags.tar /tmp
        git init
        git checkout -b gh-pages
        echo "$commit" > commit
        git add rtags.tar.gz rtags.tar.bz2 commit
        git commit -m "Release for $commit"
        git push -f git@github.com:Andersbakken/rtags-releases.git gh-pages >/dev/null
        rm -rf $RELEASES
    fi
fi

[ -n "$UNSTASH" ] && git stash pop

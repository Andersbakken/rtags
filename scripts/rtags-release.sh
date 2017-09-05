#!/bin/bash

### TO RUN
### curl https://raw.githubusercontent.com/Andersbakken/rtags/master/scripts/rtags-release.sh | bash

REPO=~/Downloads/rtags-repo
RELEASES_REPO=~/Downloads/rtags-releases
CMAKE_ARGS="-DRTAGS_ENABLE_DEV_OPTIONS=1"
FORCE=

while [ -n "$1" ]; do 
    case "$1" in
        --repo|-r)
            shift
            REPO="$1"
            ;;
        --releases-repo|-R)
            shift
            RELEASES_REPO="$1"
            ;;
        --force|-f)
            FORCE=1
            ;;
        --cmake-args|-C)
            shift
            CMAKE_ARGS="$CMAKE_ARGS $1"
            ;;
        *)
            echo "Unknown option $1"
            exit 1
            ;;
    esac
    shift
done

if [ ! -d "$REPO" ]; then
    git clone git@github.com:Andersbakken/rtags.git "$REPO" --recursive || exit 1
else
    cd "$REPO"
    git checkout -f master
    git pull --autostash || exit 1
fi

branch_name="$(git symbolic-ref HEAD 2>/dev/null)" || branch_name="(unnamed branch)"     # detached HEAD
branch_name=${branch_name##refs/heads/}
if [ "$branch_name" != "master" ]; then
    echo wrong branch $branch_name
    exit 1
fi

commit=$(git show --oneline --no-patch)
current=`curl --silent http://andersbakken.github.io/rtags-releases/commit | cut -d' ' -f1`
if [ "`echo "$commit" | cut -d' ' -f1`" = "$current" -a -z "$FORCE" ]; then
    exit 0
fi

if [ ! -d "$RELEASES_REPO" ]; then
    git clone git@github.com:Andersbakken/rtags-releases.git "$RELEASES_REPO" --recursive || exit 1
    cd "$RELEASES_REPO"
else
    cd "$RELEASES_REPO"
    git pull --autostash || exit 1
fi

if ! git branch | grep --quiet "^\* *gh-pages$"; then
    echo "wrong branch"
    exit 1
fi

rm -rf .git/hooks/*

cmake "$REPO" $CMAKE_ARGS >/dev/null 2>&1
make package_source >/dev/null
cmake "$REPO" $CMAKE_ARGS -DCPACK_GENERATOR=TBZ2 >/dev/null 2>&1
make package_source >/dev/null
echo "$commit" > commit
git add *.tar.gz *.tar.bz2 commit >/dev/null
git commit --amend -m "Release for $commit" >/dev/null
git push -f git@github.com:Andersbakken/rtags-releases.git gh-pages >/dev/null
git clean -xfd 

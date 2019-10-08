#!/bin/bash -x

echo "GOT SOME SHIT $@"
SRC=
BUILD=
INCLUDE=
LIBS=
while [ -n "$1" ]; do
    case "$1" in
        --source)
            shift
            SRC="$1"
            shift
            ;;
        --build)
            shift
            BUILD="$1"
            shift
            ;;
        -I*)
            INCLUDE=`echo $1 | sed -e 's,^-I,,'`
            shift
            ;;
        --libs)
            shift
            while [ -n "$1" ]; do
                LIBS="$LIBS $1"
                shift
            done
            ;;
        *)
            echo "Unknown option $1"
            exit 1
            ;;
    esac
done

mkdir -p "$BUILD"
cd "$BUILD"
mkdir -p v8
cd v8
ln -sf $INCLUDE $PWD/include
mkdir lib
cd lib
for i in $LIBS; do
    ln -sf $i $PWD
done
cd ..
/usr/bin/make -C $SRC lib

# echo "SRC $SRC BUILD $BUILD CXXFLAGS $CXXFLAGS LIBS $LIBS"

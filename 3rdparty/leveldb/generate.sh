#!/bin/bash
BASE=leveldb
cp leveldb.pro.base leveldb.pro
cat src/Makefile | awk '/^\t\.\/.*\.o/' | awk '{print "    src/" $1 " " $2}' | sed -e 's/\.o/.cc/' >> leveldb.pro

#!/bin/sh

test -d leveldb || git clone https://code.google.com/p/leveldb/
test -d snappy || git svn clone http://snappy.googlecode.com/svn/trunk/ snappy
cd leveldb 
git pull
cd ../snappy
git svn fetch
git svn rebase


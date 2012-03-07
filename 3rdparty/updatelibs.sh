#!/bin/sh

test -d leveldb/src || git clone https://code.google.com/p/leveldb/ leveldb/src
cd leveldb/src && git pull

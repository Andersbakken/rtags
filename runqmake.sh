#!/bin/sh

qmake INCLUDEPATH+=/usr/local/llvm/include/ "LIBS+=-L/usr/local/llvm/lib -Wl,-rpath,/usr/local/llvm/lib" -r


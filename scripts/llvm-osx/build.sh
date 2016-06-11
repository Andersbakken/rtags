#!/bin/bash
brew tap homebrew/versions
brew install llvm38 --with-libcxx --with-clang --disable-assertions --rtti $@

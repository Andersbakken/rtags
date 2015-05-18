#!/bin/bash
brew tap homebrew/versions
brew install llvm36 --with-libcxx --with-clang --disable-assertions --rtti $@

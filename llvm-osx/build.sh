#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
brew install $DIR/llvm.rb --with-clang --with-libcxx --disable-assertions $@

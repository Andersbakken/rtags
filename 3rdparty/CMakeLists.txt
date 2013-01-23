cmake_minimum_required(VERSION 2.8.4)
include(ExternalProject)
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

ExternalProject_Add(
    llvm
    SOURCE_DIR llvm
    GIT_REPOSITORY http://llvm.org/git/llvm.git
    GIT_TAG origin/master
    BINARY_DIR llvm_build
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_BINARY_DIR}/clang_install -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    )

ExternalProject_Add(
    clang
    SOURCE_DIR ./clang
    GIT_REPOSITORY http://llvm.org/git/clang.git
    GIT_TAG origin/master
    BINARY_DIR clang_build
    CMAKE_ARGS -DCLANG_PATH_TO_LLVM_BUILD=../llvm_build -DCLANG_PATH_TO_LLVM_SOURCE=../llvm -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_BINARY_DIR}/clang_install -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    DEPENDS llvm
    )

ExternalProject_Add(
    leveldb
    SOURCE_DIR ./leveldb
    GIT_REPOSITORY "https://code.google.com/p/leveldb/"
    GIT_TAG "85584d497e7b354853b72f450683d59fcf6b9c5c"
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    UPDATE_COMMAND ""
    )

add_custom_target (
    leveldb_make
    ALL
    COMMAND \$\(MAKE\)
    WORKING_DIRECTORY ./leveldb
    DEPENDS leveldb
    )

add_custom_target (
    leveldb_pull
    COMMAND git pull
    WORKING_DIRECTORY ./leveldb
    DEPENDS leveldb
    )

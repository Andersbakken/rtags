cmake_minimum_required(VERSION 2.8.4)
include(ExternalProject)
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

ExternalProject_Add(
    make-3.82
    SOURCE_DIR ./make-3.82
    CONFIGURE_COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/make-3.82/configure -prefix=${CMAKE_CURRENT_BINARY_DIR}/make_install
    BUILD_COMMAND make
    # INSTALL_COMMAND make install
    UPDATE_COMMAND ""
    )

#add_custom_target (
    #make_make
    #ALL
    #COMMAND \$\(MAKE\)
    #WORKING_DIRECTORY ./make
    #DEPENDS make
    #)

ExternalProject_Add(
    llvm
    SOURCE_DIR ./llvm
    GIT_REPOSITORY "http://llvm.org/git/llvm.git"
    #GIT_TAG "release_31"
    BINARY_DIR "llvm_build"
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_BINARY_DIR}/clang_install -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    UPDATE_COMMAND ""
    )

add_custom_target (
    llvm_pull
    COMMAND git pull
    WORKING_DIRECTORY ./llvm
    DEPENDS llvm
    )

ExternalProject_Add(
    clang
    SOURCE_DIR ./clang
    GIT_REPOSITORY "http://llvm.org/git/clang.git"
    #GIT_TAG "release_31"
    BINARY_DIR "clang_build"
    UPDATE_COMMAND ""
    CMAKE_ARGS -DCLANG_PATH_TO_LLVM_BUILD=../llvm_build -DCLANG_PATH_TO_LLVM_SOURCE=../llvm -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_BINARY_DIR}/clang_install -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    DEPENDS llvm
    )

add_custom_target (
    clang_pull
    COMMAND git pull
    WORKING_DIRECTORY ./clang
    DEPENDS clang
    )


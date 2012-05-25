cmake_minimum_required(VERSION 2.8)
include(ExternalProject)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()
#include_directories(${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
#add_subdirectory(leveldb)
#add_subdirectory(llvm)
# SET(leveldb/db/*.cc deps)
AUX_SOURCE_DIRECTORY(leveldb DEPS)
ExternalProject_Add(
    leveldb
    SOURCE_DIR ./leveldb
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    )

ExternalProject_Add_Step(leveldb leveldb_make
    WORKING_DIRECTORY ./leveldb
    COMMAND \$\(MAKE\)
    ALWAYS 1)


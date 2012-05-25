set(clang_ROOT $ENV{CLANG_ROOT})
IF("${clang_ROOT}" STREQUAL "")
  set(clang_ROOT "${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/")
ENDIF("${clang_ROOT}" STREQUAL "")

set(clang_VERSION $ENV{CLANG_VERSION})
IF("${clang_VERSION}" STREQUAL "")
  set(clang_VERSION "3.1")
ENDIF("${clang_VERSION}" STREQUAL "")

get_filename_component(PARENT_DIR ${CMAKE_CURRENT_LIST_DIR} PATH)

include_directories(${clang_ROOT}/include)
add_definitions(-DCLANG_RUNTIME_INCLUDE="${clang_ROOT}/lib/clang/${clang_VERSION}/include")
set(clang_LIBS ${clang_ROOT}/lib/liblibclang.so ${PARENT_DIR}/3rdparty/leveldb/libleveldb.a crypto)

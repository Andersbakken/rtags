cmake_minimum_required(VERSION 2.8.3)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

include(CheckCXXSymbolExists)

if(CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DRTAGS_DEBUG)
endif()

add_definitions(-Wall "-DCLANG_INCLUDEPATH=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/clang/3.1/include/\""
  "-DCLANG_BIN=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/bin/\"")

add_definitions(-DOS_${CMAKE_SYSTEM_NAME})

if(CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(system_LIBS rt)
elseif(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
  add_definitions("-DMAKE=\"gmake\"")
endif()

check_cxx_symbol_exists(backtrace "execinfo.h" HAVE_BACKTRACE)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/config.h)

include(rtags.cmake)

set(leveldb_LIBS ${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/leveldb/libleveldb.a)

include_directories(${CMAKE_CURRENT_LIST_DIR})
add_executable(rdm rdm.cpp)
target_link_libraries(rdm rtags ${leveldb_LIBS} ${clang_LIBS} ${system_LIBS})
add_dependencies(rdm rtags)

add_executable(rc rc.cpp)
target_link_libraries(rc rtags ${leveldb_LIBS} ${clang_LIBS} ${system_LIBS})
add_dependencies(rc rtags)

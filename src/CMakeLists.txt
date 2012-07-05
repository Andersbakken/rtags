cmake_minimum_required(VERSION 2.8.3)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

if(CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DRTAGS_DEBUG)
endif()

add_definitions(-Wall "-DCLANG_INCLUDEPATH=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/clang/3.1/include/\"")
add_definitions(-DOS_${CMAKE_SYSTEM_NAME})

if(CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(system_LIBS rt)
elseif(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
  add_definitions("-DMAKE=\"gmake\"")
endif()

include(rtags.cmake)

set(leveldb_LIBS ${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/leveldb/libleveldb.a)

include_directories(${CMAKE_CURRENT_LIST_DIR})
add_executable(rdm rdm-main.cpp)
target_link_libraries(rdm rtags ${leveldb_LIBS} ${clang_LIBS} ${system_LIBS})
add_dependencies(rdm rtags)

add_executable(rc rc-main.cpp)
target_link_libraries(rc rtags ${leveldb_LIBS} ${clang_LIBS} ${system_LIBS})
add_dependencies(rc rtags)

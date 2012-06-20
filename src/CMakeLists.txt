cmake_minimum_required(VERSION 2.8.3)
include(rtags.cmake)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

add_definitions(-Wall "-DCLANG_INCLUDEPATH=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/clang/3.1/include/\"")

set(leveldb_LIBS ${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/leveldb/libleveldb.a)

include_directories(${CMAKE_CURRENT_LIST_DIR})
add_executable(rdm rdm.cpp)
target_link_libraries(rdm ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY} ${clang_LIBS} rtags ${leveldb_LIBS})
add_dependencies(rdm rtags)

add_executable(rc rc.cpp)
target_link_libraries(rc ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY} rtags)
add_dependencies(rc rtags)

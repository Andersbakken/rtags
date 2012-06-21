cmake_minimum_required(VERSION 2.8.3)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

add_definitions(-Wall "-DCLANG_INCLUDEPATH=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/clang/3.1/include/\"")
add_definitions(-D${CMAKE_SYSTEM_NAME})

include(rtags.cmake)

set(leveldb_LIBS ${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/leveldb/libleveldb.a)

include_directories(${CMAKE_CURRENT_LIST_DIR})
add_executable(rdm rdm.cpp)
target_link_libraries(rdm rtags ${leveldb_LIBS} ${clang_LIBS} ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY})
add_dependencies(rdm rtags)

add_executable(rc rc.cpp)
target_link_libraries(rc rtags ${leveldb_LIBS} ${clang_LIBS} ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY})
add_dependencies(rc rtags)

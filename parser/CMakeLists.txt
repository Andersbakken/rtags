cmake_minimum_required(VERSION 2.8)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/include/clang-c)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/include)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/../src)
add_executable(parser main.cpp)
target_link_libraries(parser
  ${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/liblibclang.so
  ${CMAKE_CURRENT_BINARY_DIR}/../src/librtags.a
  rt)


cmake_minimum_required(VERSION 2.8)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/include)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/../src)
add_executable(clangtest main.cpp)
target_link_libraries(clangtest
  ${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/libclang.so
  ${CMAKE_CURRENT_BINARY_DIR}/../src/librtags.a
  rt)

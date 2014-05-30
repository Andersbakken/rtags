find_library(APPKIT_LIBRARY AppKit)

cmake_minimum_required(VERSION 2.8)
add_executable(car Car.m main.m)
target_link_libraries(car ${APPKIT_LIBRARY})

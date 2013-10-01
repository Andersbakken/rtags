set(CMAKE_LEGACY_CYGWIN_WIN32 0)

cmake_minimum_required(VERSION 2.6)
project(rtags)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")

add_subdirectory(src)

if (EXISTS "rules.ninja") 
  add_custom_target(release COMMAND cmake -GNinja -DCMAKE_BUILD_TYPE=Release . WORKING_DIRECTORY .)
  add_custom_target(debug COMMAND cmake -GNinja -DCMAKE_BUILD_TYPE=Debug . WORKING_DIRECTORY .)
else()
  add_custom_target(release COMMAND cmake -DCMAKE_BUILD_TYPE=Release . WORKING_DIRECTORY)
  add_custom_target(debug COMMAND cmake -DCMAKE_BUILD_TYPE=Debug . WORKING_DIRECTORY)
endif()

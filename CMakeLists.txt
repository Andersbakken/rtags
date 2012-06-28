cmake_minimum_required(VERSION 2.6)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

add_subdirectory(makelib)
add_subdirectory(src)

add_custom_target(3rdparty 
    COMMAND cmake . && \$\(MAKE\)
    WORKING_DIRECTORY 3rdparty)

add_custom_target(debug 
    COMMAND cmake -DCMAKE_BUILD_TYPE=Debug .
    WORKING_DIRECTORY .)

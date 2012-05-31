cmake_minimum_required(VERSION 2.6)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

add_subdirectory(makelib)
add_subdirectory(rc)
add_subdirectory(rdm)
add_subdirectory(shared)

add_custom_target(3rdparty 
    COMMAND cmake . && \$\(MAKE\)
    WORKING_DIRECTORY 3rdparty)


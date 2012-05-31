cmake_minimum_required(VERSION 2.6)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

add_definitions(-Wall -DBUILDING_RC)

include_directories(
    ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
    ${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
    .)

set(rc_SRCS main.cpp)

include(../shared/shared.cmake)
include(../shared/PCH_GCC4_v2.cmake)

#ADD_PCH_RULE(Pch.h rc_SRCS rc_PCHFLAGS)
# add_definitions(${rc_PCHFLAGS})

add_executable(rc ${rc_SRCS} ${UIS} ${RSCS} ${TRS} ${MOCS})
target_link_libraries(rc rtags ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY})

add_dependencies(rc gperf rtags)

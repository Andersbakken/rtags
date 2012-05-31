cmake_minimum_required(VERSION 2.6)
include(shared.cmake)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})
include_directories(
  ${CMAKE_CURRENT_LIST_DIR}
  ${CMAKE_CURRENT_LIST_DIR}/messages
  ${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
)

set(shared_SRCS
  ${CMAKE_CURRENT_LIST_DIR}/Client.cpp
  ${CMAKE_CURRENT_LIST_DIR}/Connection.cpp
  ${CMAKE_CURRENT_LIST_DIR}/GccArguments.cpp
  ${CMAKE_CURRENT_LIST_DIR}/Log.cpp
  ${CMAKE_CURRENT_LIST_DIR}/MakefileParser.cpp
  ${CMAKE_CURRENT_LIST_DIR}/Messages.cpp
  ${CMAKE_CURRENT_LIST_DIR}/Path.cpp
  ${CMAKE_CURRENT_LIST_DIR}/RTags.cpp
  ${CMAKE_CURRENT_LIST_DIR}/messages/AddMessage.cpp
  ${CMAKE_CURRENT_LIST_DIR}/messages/ErrorMessage.cpp
  ${CMAKE_CURRENT_LIST_DIR}/messages/QueryMessage.cpp
  )

set(shared_HDRS
  ${CMAKE_CURRENT_LIST_DIR}/Client.h
  ${CMAKE_CURRENT_LIST_DIR}/Connection.h
  ${CMAKE_CURRENT_LIST_DIR}/GccArguments.h
  ${CMAKE_CURRENT_LIST_DIR}/Log.h
  ${CMAKE_CURRENT_LIST_DIR}/MakefileParser.h
  ${CMAKE_CURRENT_LIST_DIR}/Message.h
  ${CMAKE_CURRENT_LIST_DIR}/Messages.h
  ${CMAKE_CURRENT_LIST_DIR}/Path.h
  ${CMAKE_CURRENT_LIST_DIR}/RTags.h
  ${CMAKE_CURRENT_LIST_DIR}/messages/AddMessage.h
  ${CMAKE_CURRENT_LIST_DIR}/messages/ErrorMessage.h
  ${CMAKE_CURRENT_LIST_DIR}/messages/QueryMessage.h
  )

set(shared_MOCS
  ${CMAKE_CURRENT_LIST_DIR}/Connection.h
  ${CMAKE_CURRENT_LIST_DIR}/Client.h
  ${CMAKE_CURRENT_LIST_DIR}/messages/AddMessage.h
  ${CMAKE_CURRENT_LIST_DIR}/messages/QueryMessage.h
  ${CMAKE_CURRENT_LIST_DIR}/messages/ErrorMessage.h
  ${CMAKE_CURRENT_LIST_DIR}/Message.h
  ${CMAKE_CURRENT_LIST_DIR}/MakefileParser.h
  )

set(shared_CPPMOCS
  ${CMAKE_CURRENT_LIST_DIR}/Connection.cpp
  )
QT4_DO_THE_RIGHT_THING(MOCS ${shared_MOCS})
QT4_GENERATE_MOCS(${shared_CPPMOCS})

add_custom_command(
    OUTPUT gccopts_gperf.h
    DEPENDS gccopts.gperf
    PRE_BUILD
    COMMAND gperf -I -C -l -L C++ gccopts.gperf -Z gccopts_gperf > gccopts_gperf.h
    VERBATIM
    )

add_custom_target(
    gperf
    DEPENDS gccopts_gperf.h
    )

add_library(rtags ${shared_SRCS} ${MOCS})
add_dependencies(rtags gperf)

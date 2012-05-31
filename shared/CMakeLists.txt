cmake_minimum_required(VERSION 2.8.3)
include(shared.cmake)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})
include(PCH_GCC4_v2.cmake)
include_directories(
  ${CMAKE_CURRENT_LIST_DIR}
  ${CMAKE_CURRENT_LIST_DIR}/messages
  ${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
)

set(shared_SRCS
  Client.cpp
  Connection.cpp
  GccArguments.cpp
  Log.cpp
  MakefileParser.cpp
  Messages.cpp
  Path.cpp
  RTags.cpp
  messages/AddMessage.cpp
  messages/ErrorMessage.cpp
  messages/QueryMessage.cpp
  )

set(shared_HDRS
  Client.h
  Connection.h
  GccArguments.h
  Log.h
  MakefileParser.h
  Message.h
  Messages.h
  Path.h
  RTags.h
  messages/AddMessage.h
  messages/ErrorMessage.h
  messages/QueryMessage.h
  )

set(shared_MOCS
  Connection.h
  Client.h
  messages/AddMessage.h
  messages/QueryMessage.h
  messages/ErrorMessage.h
  Message.h
  MakefileParser.h
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

add_pch_rule(Pch.h shared_SRCS shared_PCHFLAGS)
add_definitions(${shared_PCHFLAGS})

add_library(rtags ${shared_SRCS} ${MOCS})
add_dependencies(rtags gperf)

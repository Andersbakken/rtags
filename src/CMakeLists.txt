cmake_minimum_required(VERSION 2.8.6)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif ()

find_package(LibClang REQUIRED)
include_directories(${CLANG_INCLUDE})

find_package(V8 REQUIRED)
find_package(LevelDB)

set(RCT_INCLUDE_DIR rct)
include(${CMAKE_CURRENT_LIST_DIR}/rct/rct.cmake)

if (CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DRTAGS_DEBUG)
endif ()

add_definitions(-Wall)
add_definitions("-DCLANG_INCLUDEPATH=\"${CLANG_SYSTEM_INCLUDE}\"")
add_definitions("-DCLANG_VERSION_MAJOR=${CLANG_VERSION_MAJOR}")
add_definitions("-DCLANG_VERSION_MINOR=${CLANG_VERSION_MINOR}")
add_definitions("-DESPRIMA_JS=\"${PROJECT_SOURCE_DIR}/src/esprima/esprima.js\"")
add_definitions(-DOS_${CMAKE_SYSTEM_NAME})

if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(SYSTEM_LIBS pthread rt crypto)
elseif (CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
  add_definitions(-D__LONG_LONG_SUPPORTED)
  set(SYSTEM_LIBS pthread crypto)
elseif (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(SYSTEM_LIBS crypto)
  find_program(SW_VER sw_vers)
  execute_process(COMMAND "${SW_VER}" -productVersion OUTPUT_VARIABLE osver)
  string(REPLACE "\n" "" osver "${osver}")
  if (${osver} VERSION_LESS "10.7")
    message("OS X ${osver} is too old for FSEvents, falling back to kqueue")
    # else ()
    #   set(HAVE_FSEVENTS 1)
  endif ()
  find_library(CORESERVICES_LIBRARY CoreServices)
  find_path(CORESERVICES_INCLUDE "CoreServices/CoreServices.h")
  find_library(COREFOUNDATION_LIBRARY CoreFoundation)
  find_path(COREFOUNDATION_INCLUDE "CoreFoundation/CoreFoundation.h")
endif ()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

set(RTAGS_SHARED_SOURCES
  Client.cpp
  CompileMessage.cpp
  CompletionMessage.cpp
  CreateOutputMessage.cpp
  Location.cpp
  QueryMessage.cpp
  RClient.cpp
  RTags.cpp)

set(RDM_SOURCES
  CompileJob.cpp
  CompletionJob.cpp
  CursorInfo.cpp
  CursorInfoJob.cpp
  DependenciesJob.cpp
  FileManager.cpp
  FindFileJob.cpp
  FindSymbolsJob.cpp
  FollowLocationJob.cpp
  GccArguments.cpp
  IndexerJob.cpp
  JSONJob.cpp
  Job.cpp
  ListSymbolsJob.cpp
  Preprocessor.cpp
  Project.cpp
  RTagsClang.cpp
  ReferencesJob.cpp
  ScanJob.cpp
  Server.cpp
  StatusJob.cpp
  ValidateDBJob.cpp
  )

set(GR_SOURCES GRParser.cpp GRTags.cpp Location.cpp RTags.cpp)

include_directories(${CMAKE_CURRENT_LIST_DIR} ${CORESERVICES_INCLUDE} ${COREFOUNDATION_INCLUDE})

add_executable(clangtest clangtest.c)
target_link_libraries(clangtest ${CLANG_LIBS} ${CORESERVICES_LIBRARY} ${COREFOUNDATION_LIBRARY} ${SYSTEM_LIBS})

add_executable(rc rc.cpp ${RTAGS_SHARED_SOURCES} ${RCT_SOURCES})
target_link_libraries(rc ${SYSTEM_LIBS})

#if (LEVELDB_FOUND EQUAL 1)
if (0 EQUAL 1)
  add_definitions(-DGRTAGS)
  add_executable(gr ${GR_SOURCES} ${RCT_SOURCES})
  include_directories(
    ${CORESERVICES_INCLUDE}
    ${COREFOUNDATION_INCLUDE}
    ${LEVELDB_INCLUDE})
  target_link_libraries(gr
    ${CLANG_LIBS}
    ${SYSTEM_LIBS}
    ${CORESERVICES_LIBRARY}
    ${COREFOUNDATION_LIBRARY}
    ${SYSTEM_LIBS}
    ${LEVELDB_LIBS})
endif ()

add_library(rtags SHARED ${RTAGS_SHARED_SOURCES} ${RCT_SOURCES} ${RDM_SOURCES})
target_link_libraries(rtags ${rct_LIBS} ${CLANG_LIBS} ${CORESERVICES_LIBRARY} ${COREFOUNDATION_LIBRARY} ${rct_LIBS} ${SYSTEM_LIBS})

add_executable(rdm rdm.cpp)
target_link_libraries(rdm rtags)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

include_directories(${V8_INCLUDE})
if (V8_DISPOSE_HAS_ISOLATE)
  add_definitions(-DV8_DISPOSE_HAS_ISOLATE)
endif ()
add_library(rtagsesprima MODULE IndexerJobEsprima.cpp JSParser.cpp)
add_dependencies(rtagsesprima rdm)
target_link_libraries(rtagsesprima ${V8_LIBS} rtags)

add_library(rtagsclang MODULE IndexerJobClang.cpp)
add_dependencies(rtagsclang rdm)
target_link_libraries(rtagsclang rtags)

if (NOT "${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  file (GLOB binFiles "${PROJECT_SOURCE_DIR}/bin/*")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
  message("Copying files from ${PROJECT_SOURCE_DIR}/bin to ${PROJECT_BINARY_DIR}/bin")
  file (COPY ${binFiles} DESTINATION "${PROJECT_BINARY_DIR}/bin")
endif ()

if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  add_custom_command(TARGET rdm POST_BUILD
    COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_BINARY_DIR} ${PROJECT_BINARY_DIR}/bin/rdm)
  add_custom_command(TARGET rc POST_BUILD
    COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_BINARY_DIR} ${PROJECT_BINARY_DIR}/bin/rc)
  if (GRTAGS_ENABLED MATCHES 1)
    add_custom_command(TARGET gr POST_BUILD
      COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_BINARY_DIR} ${PROJECT_BINARY_DIR}/bin/gr)
  endif ()
endif ()

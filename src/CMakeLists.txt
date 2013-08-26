cmake_minimum_required(VERSION 2.8.6)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif ()

if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fpic -g -O0")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fpic -g -O0")
endif ()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++0x -g -O0")
if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
endif ()

find_package(LibClang REQUIRED)
find_package(LLVM REQUIRED)
find_package(Clang REQUIRED)
include_directories(${CLANG_INCLUDE})
include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})

find_package(V8)
find_package(YAJL)

find_package(PkgConfig)

pkg_check_modules( GAMIN gamin )
if (GAMIN_FOUND EQUAL 1)
  set(HAVE_FAM 1)
  add_definitions(${GAMIN_CFLAGS})
endif ()

add_subdirectory(rct)
include(${CMAKE_CURRENT_LIST_DIR}/rct/compiler.cmake)
# set(RCT_INCLUDE_DIR rct)
# include(${CMAKE_CURRENT_LIST_DIR}/rct/rct.cmake)

if (CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DRTAGS_DEBUG)
endif ()

add_definitions(-Wall)
add_definitions("-DCLANG_INCLUDEPATH=\"${CLANG_SYSTEM_INCLUDE}\"")
add_definitions("-DCLANG_VERSION_MAJOR=${CLANG_VERSION_MAJOR}")
add_definitions("-DCLANG_VERSION_MINOR=${CLANG_VERSION_MINOR}")
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
  CompileMessage.cpp
  CompletionMessage.cpp
  CreateOutputMessage.cpp
  Location.cpp
  QueryMessage.cpp
  RTags.cpp)

add_library(shared ${RTAGS_SHARED_SOURCES})

set(RDM_SOURCES
  CompilerManager.cpp
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

include_directories(${CMAKE_CURRENT_LIST_DIR}
                    ${CORESERVICES_INCLUDE}
                    ${COREFOUNDATION_INCLUDE}
                    rct/include/
                    rct
                    ${CMAKE_CURRENT_BINARY_DIR}/rct/include/rct)

link_directories(${LLVM_LIBRARY_DIRS})

add_executable(clangtest clangtest.c)
target_link_libraries(clangtest
${LLVM_LDFLAGS}
-Wl,--start-group
${CLANG_LIBS}
${LLVM_LIBNAMES}
-Wl,--end-group
-lstdc++
${CORESERVICES_LIBRARY} ${COREFOUNDATION_LIBRARY} ${SYSTEM_LIBS})

add_executable(rc rc.cpp RClient.cpp)
add_dependencies(rc rct shared)
target_link_libraries(rc shared rct ${SYSTEM_LIBS})

if (V8_FOUND EQUAL 1)
  list(APPEND RDM_SOURCES JSONParser.cpp)
  list(APPEND SYSTEM_LIBS ${V8_LIBS})
  include_directories(${V8_INCLUDE})

  if (V8_DISPOSE_HAS_ISOLATE)
    add_definitions(-DV8_DISPOSE_HAS_ISOLATE)
  endif ()
  if (V8_NEW_CONTEXT_TAKES_ISOLATE)
    add_definitions(-DV8_NEW_CONTEXT_TAKES_ISOLATE)
  endif ()
  add_definitions(-DHAVE_V8)
  add_definitions("-DESPRIMA_JS=\"${PROJECT_BINARY_DIR}/src/rtags-esprima.js\"")
elseif (YAJL_FOUND EQUAL 1)
  list(APPEND RDM_SOURCES JSONParser.cpp)
  list(APPEND SYSTEM_LIBS ${YAJL_LIBRARY})
  include_directories(${YAJL_INCLUDE})
  add_definitions(-DHAVE_YAJL)
endif ()

add_library(rtags SHARED ${RDM_SOURCES})
target_link_libraries(rtags
  -L.
  -L./rct/
  ${LLVM_LDFLAGS}
  -Wl,--start-group
  ${CLANG_LIBS}
  ${LLVM_LIBNAMES}
  -Wl,--end-group
  -lstdc++
  ${CORESERVICES_LIBRARY} ${COREFOUNDATION_LIBRARY} 
  -lshared
  -lrct
  -L/usr/local/lib
  -levent_pthreads
  -levent
  ${GAMIN_LDFLAGS}
  ${SYSTEM_LIBS})
add_dependencies(rtags rct)

add_executable(rdm rdm.cpp
  ${RDM_SOURCES})
target_link_libraries(rdm
  -Lsrc/
#  rtags
  shared
  rct
  -Wl,--start-group
  ${CLANG_LIBS}
  ${LLVM_LIBNAMES}
  -Wl,--end-group
  -lstdc++
)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

#add_library(rtagsclang MODULE IndexerJobClang.cpp)
add_library(rtagsclang SHARED IndexerJobClang.cpp)
add_dependencies(rtagsclang rdm)
target_link_libraries(rtagsclang
  -L.
  -L./rct/
  ${LLVM_LDFLAGS}
  -Wl,--start-group
#  ${CLANG_LIBS}
#  ${LLVM_LIBNAMES}
${CLANG_AMAZING}
${LLVM_LIBS}
  -Wl,--end-group
  -lrtags
  -lrct
)

if (V8_FOUND EQUAL 1)
  add_library(rtagsesprima MODULE IndexerJobEsprima.cpp JSParser.cpp)
  add_dependencies(rtagsesprima rtags)
  target_link_libraries(rtagsesprima rtags)

  add_custom_target(rtags-esprima.js ALL
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/concat-js.sh ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/estraverse/estraverse.js
        ${CMAKE_CURRENT_SOURCE_DIR}/escope/escope.js
        ${CMAKE_CURRENT_SOURCE_DIR}/esrefactor/lib/esrefactor.js
        ${CMAKE_CURRENT_SOURCE_DIR}/rtags.js
  )
endif()

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

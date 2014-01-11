cmake_minimum_required(VERSION 2.8.6)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif ()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fstack-protector-all")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wstack-protector")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstack-protector-all")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wstack-protector")

include(${CMAKE_SOURCE_DIR}/cmake/EnsureLibraries.cmake)

find_package(LibClang REQUIRED)
find_package(LLVM REQUIRED)
find_package(Clang REQUIRED)
include_directories(${CLANG_INCLUDE})
include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})

if (EXISTS ${CLANG_COMPILATION_INCLUDE})
  add_definitions(-DHAVE_CXCOMPILATIONDATABASE)
endif ()

if (RTAGS_BUILD_ESPRIMA)
  find_package(V8)
endif()

add_subdirectory(rct)

include(${CMAKE_CURRENT_LIST_DIR}/rct/compiler.cmake)
# set(RCT_INCLUDE_DIR rct)
# include(${CMAKE_CURRENT_LIST_DIR}/rct/rct.cmake)

if (CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions("-DRTAGS_DEBUG")
  set(RCT_EVENTLOOP_CALLBACK_TIME_THRESHOLD 2000)
endif ()

add_definitions("-Wall")
add_definitions("-DCLANG_INCLUDEPATH=\"${CLANG_SYSTEM_INCLUDE}\"")
add_definitions("-DCLANG_VERSION_MAJOR=${CLANG_VERSION_MAJOR}")
add_definitions("-DCLANG_VERSION_MINOR=${CLANG_VERSION_MINOR}")
add_definitions("-DOS_${CMAKE_SYSTEM_NAME}")

if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(SYSTEM_LIBS pthread rt crypto m dl tinfo)
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
endif ()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
add_definitions("-DRTAGS_BIN=\"${PROJECT_BINARY_DIR}/bin/\"")

set(RTAGS_SHARED_SOURCES
  CompileMessage.cpp
  CreateOutputMessage.cpp
  Location.cpp
  QueryMessage.cpp
  RTags.cpp
  Source.cpp)

add_library(shared SHARED ${RTAGS_SHARED_SOURCES})
target_link_libraries(shared rct)

set(RDM_SOURCES
  CompletionThread.cpp
  CursorInfo.cpp
  CursorInfoJob.cpp
  DependenciesJob.cpp
  FileManager.cpp
  FindFileJob.cpp
  FindSymbolsJob.cpp
  FollowLocationJob.cpp
  IndexerJob.cpp
  Job.cpp
  JobRequestMessage.cpp
  JobResponseMessage.cpp
  ListSymbolsJob.cpp
  PreprocessJob.cpp
  Preprocessor.cpp
  Project.cpp
  RTagsClang.cpp
  ReferencesJob.cpp
  ScanThread.cpp
  Server.cpp
  StatusJob.cpp
  mongoose/mongoose.c
  )

include_directories(${CMAKE_CURRENT_LIST_DIR}
                    rct/include/
                    rct
                    ${CMAKE_CURRENT_BINARY_DIR}/rct/include/rct)

link_directories(${LLVM_LIBRARY_DIRS})

if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(START_GROUP "")
  set(END_GROUP "")
else ()
  set(START_GROUP "-Wl,--start-group")
  set(END_GROUP "-Wl,--end-group")
endif ()

# add_executable(clangtest clangtest.c)
# target_link_libraries(clangtest
#   ${LLVM_LDFLAGS}
#   ${START_GROUP}
#   ${CLANG_LIBS}
#   ${LLVM_LIBNAMES}
#   ${END_GROUP}
#   -lstdc++
#   ${SYSTEM_LIBS}
# )

add_executable(rc rc.cpp RClient.cpp)
add_dependencies(rc rct shared)
target_link_libraries(rc shared rct ${SYSTEM_LIBS})

if (V8_FOUND EQUAL 1)
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
endif ()

add_library(rtags SHARED ${RDM_SOURCES})
target_link_libraries(rtags
  rct
  shared
  ${LLVM_LDFLAGS}
  ${START_GROUP}
  ${CLANG_LIBS}
  ${LLVM_LIBNAMES}
  ${END_GROUP}
  -lstdc++
  ${SYSTEM_LIBS})
add_dependencies(rtags rct)

add_executable(rdm rdm.cpp
#  ${RDM_SOURCES}
)
target_link_libraries(rdm
  rtags
  -lstdc++
  ${SYSTEM_LIBS}
)

add_executable(rp rp.cpp ClangIndexer.cpp)
target_link_libraries(rp rtags shared)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

# add_library(rtagsclang SHARED IndexerJobClang.cpp)
# add_dependencies(rtagsclang rct)
# add_dependencies(rtagsclang rdm)
# target_link_libraries(rtagsclang
#   rtags
#   -L.
# )

# if (V8_FOUND EQUAL 1)
#   add_library(rtagsesprima MODULE IndexerJobEsprima.cpp JSParser.cpp)
#   add_dependencies(rtagsesprima rtags)
#   target_link_libraries(rtagsesprima rtags)

#   set(RTAGS_ESPRIMA_FILES
#       ${CMAKE_CURRENT_SOURCE_DIR}/esprima/esprima.js
#       ${CMAKE_CURRENT_SOURCE_DIR}/estraverse/estraverse.js
#       ${CMAKE_CURRENT_SOURCE_DIR}/escope/escope.js
#       ${CMAKE_CURRENT_SOURCE_DIR}/esrefactor/lib/esrefactor.js
#       ${CMAKE_CURRENT_SOURCE_DIR}/rtags.js)
#   add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/rtags-esprima.js
#                      DEPENDS ${RTAGS_ESPRIMA_FILES}
#                      COMMAND ${CMAKE_COMMAND} -DFILES="${RTAGS_ESPRIMA_FILES}" -DTARGET="${CMAKE_CURRENT_BINARY_DIR}/rtags-esprima.js" -P "${PROJECT_SOURCE_DIR}/cmake/concat.cmake"
#                      COMMENT "Sourceing rtags-esprima.js")
#   add_custom_target(rtags-esprima ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/rtags-esprima.js)
# endif()

if (CYGWIN)
  EnsureLibraries(rdm rct)
endif ()

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
endif ()

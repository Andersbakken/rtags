cmake_minimum_required(VERSION 2.8.6)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif ()

SET(CMAKE_SKIP_BUILD_RPATH            FALSE)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH    FALSE)
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# Set RPATH when installing to a custom (non-system) directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
  SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
ENDIF("${isSystemDir}" STREQUAL "-1")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
if (NOT CYGWIN)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fstack-protector-all")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wstack-protector")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstack-protector-all")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wstack-protector")
endif ()

include(${CMAKE_SOURCE_DIR}/cmake/EnsureLibraries.cmake)

if (NOT DEFINED CLANG_LIBS)
  set(CLANG_LIBS $ENV{CLANG_LIBS})
endif()

if (NOT DEFINED CLANG_CXXFLAGS)
  set(CLANG_CXXFLAGS $ENV{CLANG_CXXFLAGS})
endif()

if (NOT DEFINED CLANG_LIBDIR)
  set(CLANG_LIBDIR $ENV{CLANG_LIBDIR})
endif()

if (NOT DEFINED CLANG_LIBS OR NOT DEFINED CLANG_CXXFLAGS OR NOT DEFINED CLANG_LIBDIR)
  find_package(LibClang REQUIRED)
endif ()

find_package(Curses REQUIRED)

set(CMAKE_CXX_FLAGS ${CLANG_CXXFLAGS})

if (EXISTS ${CLANG_COMPILATION_INCLUDE})
  add_definitions(-DHAVE_CXCOMPILATIONDATABASE)
endif ()

set(RCT_STATIC 1)
set(RCT_NO_INSTALL 1)
add_subdirectory(rct)

include(${CMAKE_CURRENT_LIST_DIR}/rct/compiler.cmake)

if (CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions("-DRTAGS_DEBUG")
  set(RCT_EVENTLOOP_CALLBACK_TIME_THRESHOLD 2000)
endif ()

add_definitions("-Wall")
add_definitions("-DCLANG_LIBDIR=${CLANG_LIBDIR}")
add_definitions("-DOS_${CMAKE_SYSTEM_NAME}")

if (CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
  add_definitions(-D__LONG_LONG_SUPPORTED)
endif ()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
add_definitions("-DRTAGS_BIN=\"${PROJECT_BINARY_DIR}/bin/\"")

set(RTAGS_SOURCES
  IndexMessage.cpp
  Location.cpp
  Symbol.cpp
  QueryMessage.cpp
  RTags.cpp)

add_library(rtags STATIC ${RTAGS_SOURCES})

include_directories(${CMAKE_CURRENT_LIST_DIR}
  rct/include/
  rct
  ${CMAKE_CURRENT_BINARY_DIR}/rct/include/rct)

if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(START_GROUP "")
  set(END_GROUP "")
else ()
  set(START_GROUP "-Wl,--start-group")
  set(END_GROUP "-Wl,--end-group")
endif ()

add_executable(rc rc.cpp RClient.cpp)
target_link_libraries(rc rtags rct)

set(RTAGS_CLANG_LIBRARIES rtags rct ${START_GROUP} ${CLANG_LIBS} ${END_GROUP} -lstdc++ ${CURSES_CURSES_LIBRARY})
set(RTAGS_CLANG_SOURCES RTagsClang.cpp Source.cpp)

add_executable(rdm
  rdm.cpp
  ClassHierarchyJob.cpp
  CompilerManager.cpp
  CompletionThread.cpp
  SymbolInfoJob.cpp
  DependenciesJob.cpp
  DumpThread.cpp
  FileManager.cpp
  FindFileJob.cpp
  FindSymbolsJob.cpp
  FollowLocationJob.cpp
  IncludeFileJob.cpp
  IndexerJob.cpp
  JobScheduler.cpp
  ListSymbolsJob.cpp
  Preprocessor.cpp
  Project.cpp
  QueryJob.cpp
  ReferencesJob.cpp
  ScanThread.cpp
  Server.cpp
  StatusJob.cpp
  Symbol.cpp
  ${RTAGS_CLANG_SOURCES})

target_link_libraries(rdm ${RTAGS_CLANG_LIBRARIES})

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

add_executable(rp rp.cpp ClangIndexer.cpp ${RTAGS_CLANG_SOURCES})
target_link_libraries(rp ${RTAGS_CLANG_LIBRARIES})

if (CYGWIN)
  EnsureLibraries(rdm rct)
endif ()

if (NOT "${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  file (GLOB binFiles "${PROJECT_SOURCE_DIR}/bin/*")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
  message("Copying files from ${PROJECT_SOURCE_DIR}/bin to ${PROJECT_BINARY_DIR}/bin")
  file (COPY ${binFiles} DESTINATION "${PROJECT_BINARY_DIR}/bin")
endif ()

install(CODE "message(\"Installing rtags...\")")
install(TARGETS rdm rc rp RUNTIME DESTINATION bin COMPONENT rtags)
install(FILES ../man/man7/rc.7 ../man/man7/rdm.7 DESTINATION share/man/man7/)

cmake_minimum_required(VERSION 2.8.6)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

include(${CMAKE_CURRENT_LIST_DIR}/ClangVersion.cmake)

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/rct/CMakeLists.txt)
  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/rct/)
  include_directories(${CMAKE_CURRENT_SOURCE_DIR}/rct/src/ ${CMAKE_CURRENT_SOURCE_DIR}/rct/include)
  set(rct_LIBS ${CMAKE_CURRENT_SOURCE_DIR}/rct/librct.a)
else()
  set(rct_ROOT ${PROJECT_SOURCE_DIR}/3rdparty/rct_install/)
  include_directories(${rct_ROOT}/include)
  set(rct_LIBS ${rct_ROOT}/lib/librct.a)
  set(rct_SOURCES "")
endif()

if(CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DRTAGS_DEBUG)
endif()

ClangVersion("${PROJECT_SOURCE_DIR}/3rdparty/clang_install" CLANG_VERSION)
message("Using Clang version ${CLANG_VERSION}")

add_definitions(-Wall)
add_definitions("-DCLANG_INCLUDEPATH=\"${PROJECT_SOURCE_DIR}/3rdparty/clang_install/lib/clang/${CLANG_VERSION}/include/\"")
add_definitions("-DCLANG_BIN=\"${PROJECT_SOURCE_DIR}/3rdparty/clang_install/bin/\"")
add_definitions(-DOS_${CMAKE_SYSTEM_NAME})

if(CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(system_LIBS rt crypto)
elseif(CMAKE_SYSTEM_NAME MATCHES "FreeBSD")
#  add_definitions("-DMAKE=\"gmake\"")
  add_definitions(-D__LONG_LONG_SUPPORTED)
  set(system_LIBS pthread crypto)
elseif(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(system_LIBS crypto)
  find_program(SW_VER sw_vers)
  execute_process(COMMAND "${SW_VER}" -productVersion OUTPUT_VARIABLE osver)
  string(REPLACE "\n" "" osver "${osver}")
  if(${osver} VERSION_LESS "10.7")
    message("OS X ${osver} is too old for FSEvents, falling back to kqueue")
  # else()
  #   set(HAVE_FSEVENTS 1)
  endif()
  find_library(CORESERVICES_LIBRARY CoreServices)
  find_path(CORESERVICES_INCLUDE "CoreServices/CoreServices.h")
  find_library(COREFOUNDATION_LIBRARY CoreFoundation)
  find_path(COREFOUNDATION_INCLUDE "CoreFoundation/CoreFoundation.h")
endif()

include(CheckCXXCompilerFlag)
if(NOT CMAKE_SYSTEM_NAME MATCHES "Darwin")
  CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX_0X)
  if(COMPILER_SUPPORTS_CXX_0X)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
  endif()
else()
  add_definitions(-D_DARWIN_UNLIMITED_SELECT)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
include(clang.cmake)

include_directories(${PROJECT_SOURCE_DIR}/3rdparty/leveldb)

set(rtags_shared_SOURCES
    Client.cpp
    CompileMessage.cpp
    CompilerManager.cpp
    CompletionMessage.cpp
    Connection.cpp
    CreateOutputMessage.cpp
    LocalClient.cpp
    Location.cpp
    Messages.cpp
    QueryMessage.cpp
    RClient.cpp
    RTags.cpp
    )

set(rdm_SOURCES
    ${rtags_shared_SOURCES}
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
    LocalServer.cpp
    Preprocessor.cpp
    Project.cpp
    RTagsClang.cpp
    ReferencesJob.cpp
    ScanJob.cpp
    Server.cpp
    StatusJob.cpp
    ValidateDBJob.cpp
    )

set(gr_SOURCES
    GRParser.cpp
    GRTags.cpp
    Location.cpp
    RTags.cpp
)

include_directories(${CMAKE_CURRENT_LIST_DIR} ${CORESERVICES_INCLUDE} ${COREFOUNDATION_INCLUDE})
add_executable(rdm rdm.cpp ${rtags_shared_SOURCES} ${rdm_SOURCES} ${rct_SOURCES})
target_link_libraries(rdm ${rct_LIBS} ${clang_LIBS} ${CORESERVICES_LIBRARY} ${COREFOUNDATION_LIBRARY} ${rct_LIBS} ${system_LIBS} ${rct_LIBS})

add_executable(clangtest clangtest.c)
target_link_libraries(clangtest ${clang_LIBS} ${CORESERVICES_LIBRARY} ${COREFOUNDATION_LIBRARY} ${rct_LIBS} ${system_LIBS})

add_executable(rc rc.cpp ${rtags_shared_SOURCES} ${rct_SOURCES})
target_link_libraries(rc ${rct_LIBS} ${system_LIBS})

add_executable(gr ${gr_SOURCES} ${rct_SOURCES})
include_directories(
  ${CORESERVICES_INCLUDE}
  ${COREFOUNDATION_INCLUDE}
  ${PROJECT_SOURCE_DIR}/3rdparty/leveldb/include)
target_link_libraries(gr
  ${clang_LIBS}
  ${system_LIBS}
  ${CORESERVICES_LIBRARY}
  ${COREFOUNDATION_LIBRARY}
  ${rct_LIBS}
  ${PROJECT_SOURCE_DIR}/3rdparty/leveldb/libleveldb.a)

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  add_custom_command(TARGET rdm POST_BUILD
                     COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_SOURCE_DIR} rdm)
  add_custom_command(TARGET rc POST_BUILD
                     COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_SOURCE_DIR} rc)
  add_custom_command(TARGET gr POST_BUILD
                     COMMAND ${CMAKE_CURRENT_LIST_DIR}/fix-dylib.sh ${PROJECT_SOURCE_DIR} gr)
endif()

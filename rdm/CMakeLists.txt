cmake_minimum_required(VERSION 2.8.3)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif(NOT CMAKE_BUILD_TYPE)

add_definitions(-Wall "-DCLANG_INCLUDEPATH=\"${CMAKE_CURRENT_BINARY_DIR}/../3rdparty/clang_install/lib/clang/3.1/include/\"")

include_directories(
    ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
    ${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ../3rdparty/leveldb/include)

set(rdm_HDRS
    Indexer.h
    SHA256.h
    Rdm.h
    IndexerJob.h
    Source.h
    DirtyJob.h
    Job.h
    CursorInfo.h
    AbortInterface.h
    MemoryMonitor.h
    Database.h
    Location.h
    )

set(rdm_SRCS
    main.cpp
    Indexer.cpp
    Server.cpp
    SHA256.cpp
    DumpJob.cpp
    FollowLocationJob.cpp
    CursorInfoJob.cpp
    ListSymbolsJob.cpp
    FindSymbolsJob.cpp
    ReferencesJob.cpp
    Rdm.cpp
    StatusJob.cpp
    IndexerJob.cpp
    DirtyJob.cpp
    Job.cpp
    TestJob.cpp
    MemoryMonitor.cpp
    Database.cpp
    Location.cpp
    )

set(rdm_MOCS
    Server.h
    DumpJob.h
    FollowLocationJob.h
    CursorInfoJob.h
    ListSymbolsJob.h
    FindSymbolsJob.h
    ReferencesJob.h
    StatusJob.h
    TestJob.h
    IndexerJob.h
    Job.h
    Indexer.h
    Rdm.h
    )

include(../shared/shared.cmake)
include(../shared/clang.cmake)
include(../shared/PCH_GCC4_v2.cmake)

QT4_DO_THE_RIGHT_THING(MOCS ${rdm_MOCS})

add_pch_rule(Pch.h rdm_SRCS rdm_PCHFLAGS)
add_definitions(${rdm_PCHFLAGS})

add_executable(rdm ${rdm_SRCS} ${MOCS})
add_dependencies(rdm rtags)
target_link_libraries(rdm ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY} ${clang_LIBS} rtags)

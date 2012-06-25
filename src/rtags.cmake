cmake_minimum_required(VERSION 2.8.3)
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})
include(PCH_GCC4_v2.cmake)
include_directories(
  ${CMAKE_CURRENT_LIST_DIR}
  ${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
)

include_directories(
    ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
    ${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ../signalslot
    ../3rdparty/leveldb/include)

set(rtags_HDRS
    AbortInterface.h
    ByteArray.h
    Client.h
    Connection.h
    CursorInfo.h
    CursorInfoJob.h
    Database.h
    DirtyJob.h
    ErrorMessage.h
    EventObject.h
    FileSystemWatcher.h
    FindSymbolsJob.h
    FollowLocationJob.h
    GccArguments.h
    Indexer.h
    IndexerJob.h
    Job.h
    List.h
    ListSymbolsJob.h
    Location.h
    Log.h
    LogObject.h
    MakefileMessage.h
    MakefileParser.h
    Map.h
    MemoryMonitor.h
    Message.h
    Messages.h
    Mutex.h
    MutexLocker.h
    OutputMessage.h
    Path.h
    Pch.h
    QueryMessage.h
    RTags.h
    Rdm.h
    ReadLocker.h
    ReadWriteLock.h
    ReferencesJob.h
    RegExp.h
    ResponseMessage.h
    RunTestJob.h
    SHA256.h
    ScopedDB.h
    Serializer.h
    Server.h
    Set.h
    Source.h
    StatusJob.h
    Str.h
    TestJob.h
    Thread.h
    ThreadPool.h
    WaitCondition.h
    WriteLocker.h
    EventLoop.h
    EventReceiver.h
    Event.h
    LocalClient.h
    LocalServer.h
    )

set(rtags_SRCS
    Client.cpp
    Connection.cpp
    CursorInfoJob.cpp
    Database.cpp
    DirtyJob.cpp
    ErrorMessage.cpp
    EventObject.cpp
    FileSystemWatcher.cpp
    FindSymbolsJob.cpp
    FollowLocationJob.cpp
    GccArguments.cpp
    Indexer.cpp
    IndexerJob.cpp
    Job.cpp
    ListSymbolsJob.cpp
    Location.cpp
    Log.cpp
    MakefileMessage.cpp
    MakefileParser.cpp
    MemoryMonitor.cpp
    Messages.cpp
    OutputMessage.cpp
    Path.cpp
    QueryMessage.cpp
    RTags.cpp
    Rdm.cpp
    ReadWriteLock.cpp
    ReferencesJob.cpp
    RunTestJob.cpp
    SHA256.cpp
    ScopedDB.cpp
    Server.cpp
    StatusJob.cpp
    TestJob.cpp
    Thread.cpp
    ThreadPool.cpp
    EventLoop.cpp
    LocalClient.cpp
    LocalServer.cpp
)

set(rtags_MOCS
    Client.h
    Connection.h
    ErrorMessage.h
    EventObject.h
    FileSystemWatcher.h
    Indexer.h
    IndexerJob.h
    LogObject.h
    MakefileMessage.h
    MakefileParser.h
    Message.h
    OutputMessage.h
    QueryMessage.h
    ResponseMessage.h
    Server.h
    RunTestJob.h
    )

include(qt4.cmake)
include(clang.cmake)
include(PCH_GCC4_v2.cmake)

set(rtags_CPPMOCS
  ${CMAKE_CURRENT_LIST_DIR}/Connection.cpp
  )
QT4_DO_THE_RIGHT_THING(MOCS ${rtags_MOCS})
QT4_GENERATE_MOCS(${rtags_CPPMOCS})

add_pch_rule(Pch.h rtags_SRCS rtags_PCHFLAGS)
add_definitions(${rtags_PCHFLAGS})

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

add_library(rtags ${rtags_SRCS} ${MOCS})
add_dependencies(rtags gperf)

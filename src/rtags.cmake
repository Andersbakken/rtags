cmake_minimum_required(VERSION 2.8.3)
include(PCH_GCC4_v2.cmake)
include_directories(
  ${CMAKE_CURRENT_LIST_DIR}
)

include_directories(
    ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ../3rdparty/leveldb/include)

set(rtags_HDRS
    AbortInterface.h
    ByteArray.h
    Client.h
    Connection.h
    CursorInfo.h
    CursorInfoJob.h
    ErrorMessage.h
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
    CreateOutputMessage.h
    Path.h
    Pch.h
    QueryMessage.h
    RTags.h
    ReadLocker.h
    ReadWriteLock.h
    ReferencesJob.h
    RegExp.h
    ResponseMessage.h
    RunTestJob.h
    SHA256.h
    Serializer.h
    Server.h
    Set.h
    Source.h
    StatusJob.h
    ValidateDBJob.h
    GRScanJob.h
    FindFileJob.h
    Str.h
    TestJob.h
    Thread.h
    ThreadPool.h
    ThreadLocal.h
    WaitCondition.h
    WriteLocker.h
    EventLoop.h
    EventReceiver.h
    Event.h
    LocalClient.h
    LocalServer.h
    Process.h
    ParseJob.h
    GRTags.h
    GRParser.h
    GRParseJob.h
    GRTagsMessage.h
    Project.h
    FastDelegate.h
    SignalSlot.h
    )

set(rtags_SRCS
    Client.cpp
    Connection.cpp
    CursorInfoJob.cpp
    ErrorMessage.cpp
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
    CreateOutputMessage.cpp
    Path.cpp
    QueryMessage.cpp
    RTags.cpp
    ReadWriteLock.cpp
    ReferencesJob.cpp
    RunTestJob.cpp
    SHA256.cpp
    Server.cpp
    StatusJob.cpp
    ValidateDBJob.cpp
    GRScanJob.cpp
    FindFileJob.cpp
    TestJob.cpp
    Thread.cpp
    ThreadPool.cpp
    EventLoop.cpp
    LocalClient.cpp
    LocalServer.cpp
    Process.cpp
    ParseJob.cpp
    CursorInfo.cpp
    GRTags.cpp
    GRParser.cpp
    GRParseJob.cpp
    GRTagsMessage.cpp
    Project.cpp
)

include(clang.cmake)
include(PCH_GCC4_v2.cmake)

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

add_library(rtags ${rtags_SRCS})
add_dependencies(rtags gperf)


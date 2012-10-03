cmake_minimum_required(VERSION 2.8.3)
include(clang.cmake)
include(PCH_GCC4_v2.cmake)
include_directories(
  ${CMAKE_CURRENT_LIST_DIR}
)

include_directories(
    ${CMAKE_SOURCE_DIR} 
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}../3rdparty/picojson
    )


set(rtags_HDRS
    AbortInterface.h
    ByteArray.h
    Client.h
    Connection.h
    CreateOutputMessage.h
    CursorInfo.h
    CursorInfoJob.h
    Event.h
    EventLoop.h
    EventReceiver.h
    FastDelegate.h
    FileManager.h
    FileSystemWatcher.h
    FindFileJob.h
    FindSymbolsJob.h
    FollowLocationJob.h
    GRParseJob.h
    GRParser.h
    GRScanJob.h
    GRTags.h
    GccArguments.h
    Indexer.h
    IndexerJob.h
    IniFile.h
    Job.h
    List.h
    ListSymbolsJob.h
    LocalClient.h
    LocalServer.h
    Location.h
    Log.h
    LogObject.h
    MakefileParser.h
    Map.h
    MemoryMonitor.h
    Message.h
    Messages.h
    Mutex.h
    MutexLocker.h
    Path.h
    Pch.h
    Preprocessor.h
    Process.h
    Project.h
    ProjectMessage.h
    QueryMessage.h
    RClient.h
    RTags.h
    ReadLocker.h
    ReadWriteLock.h
    ReferencesJob.h
    RegExp.h
    ResponseMessage.h
    Semaphore.h
    Serializer.h
    Server.h
    Set.h
    SharedMemory.h
    SignalSlot.h
    SourceInformation.h
    StatusJob.h
    Str.h
    TestJob.h
    Thread.h
    ThreadLocal.h
    ThreadPool.h
    ValidateDBJob.h
    WaitCondition.h
    WriteLocker.h
    )

set(rtags_SRCS
    Client.cpp
    Connection.cpp
    CreateOutputMessage.cpp
    CursorInfo.cpp
    CursorInfoJob.cpp
    EventLoop.cpp
    FileManager.cpp
    FindFileJob.cpp
    FindSymbolsJob.cpp
    FollowLocationJob.cpp
    GRParseJob.cpp
    GRParser.cpp
    GRScanJob.cpp
    GRTags.cpp
    GccArguments.cpp
    Indexer.cpp
    IndexerJob.cpp
    IniFile.cpp
    Job.cpp
    ListSymbolsJob.cpp
    LocalClient.cpp
    LocalServer.cpp
    Location.cpp
    Log.cpp
    MakefileParser.cpp
    MemoryMonitor.cpp
    Messages.cpp
    Path.cpp
    Preprocessor.cpp
    Process.cpp
    Project.cpp
    ProjectMessage.cpp
    QueryMessage.cpp
    RClient.cpp
    RTags.cpp
    ReadWriteLock.cpp
    ReferencesJob.cpp
    Semaphore.cpp
    Server.cpp
    SharedMemory.cpp
    StatusJob.cpp
    TestJob.cpp
    Thread.cpp
    ThreadPool.cpp
    ValidateDBJob.cpp
)

if(HAVE_INOTIFY EQUAL 1)
  list(APPEND rtags_SRCS FileSystemWatcher_inotify.cpp)
elseif(HAVE_FSEVENTS EQUAL 1)
  list(APPEND rtags_SRCS FileSystemWatcher_fsevents.cpp)
elseif(HAVE_KQUEUE EQUAL 1)
  list(APPEND rtags_SRCS FileSystemWatcher_kqueue.cpp)
endif()


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


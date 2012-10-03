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

set(rtags_client_HDRS
    AbortInterface.h
    ByteArray.h
    Client.h
    Connection.h
    CreateOutputMessage.h
    Event.h
    EventLoop.h
    EventReceiver.h
    FastDelegate.h
    IniFile.h
    Job.h
    List.h
    LocalClient.h
    Location.h
    Log.h
    LogObject.h
    Map.h
    Message.h
    Messages.h
    Mutex.h
    MutexLocker.h
    Path.h
    Pch.h
    Preprocessor.h
    Process.h
    ProjectMessage.h
    QueryMessage.h
    RClient.h
    ReadLocker.h
    RegExp.h
    ResponseMessage.h
    Semaphore.h
    Serializer.h
    Set.h
    SharedMemory.h
    SignalSlot.h
    SourceInformation.h
    ReadWriteLock.h
    Str.h
    Thread.h
    ThreadLocal.h
    ThreadPool.h
    WaitCondition.h
    WriteLocker.h
    RTags.h
   )

set(rtags_client_SRCS
    Client.cpp
    Connection.cpp
    CreateOutputMessage.cpp
    EventLoop.cpp
    LocalClient.cpp
    Location.cpp
    Log.cpp
    Messages.cpp
    Path.cpp
    Preprocessor.cpp
    Process.cpp
    ProjectMessage.cpp
    QueryMessage.cpp
    RClient.cpp
    ReadWriteLock.cpp
    Semaphore.cpp
    SharedMemory.cpp
    Thread.cpp
    ThreadPool.cpp
    RTags.cpp
    )

set(rtags_HDRS
    ${rtags_client_HDRS}
    FindFileJob.h
    CursorInfoJob.h
    FindSymbolsJob.h
    FollowLocationJob.h
    GRParseJob.h
    GRScanJob.h
    IndexerJob.h
    ListSymbolsJob.h
    ReferencesJob.h
    StatusJob.h
    TestJob.h
    ValidateDBJob.h
    Server.h
    FileSystemWatcher.h
    GccArguments.h
    LocalServer.h
    MemoryMonitor.h
    MakefileParser.h
    CursorInfo.h
    GRParser.h
    GRTags.h
    Indexer.h
    FileManager.h
    Project.h
    RTagsClang.h
    )

set(rtags_SRCS
    ${rtags_client_SRCS}
    CursorInfoJob.cpp
    FindFileJob.cpp
    FindSymbolsJob.cpp
    FollowLocationJob.cpp
    GRParseJob.cpp
    GRScanJob.cpp
    IndexerJob.cpp
    IniFile.cpp
    Job.cpp
    ListSymbolsJob.cpp
    ReferencesJob.cpp
    StatusJob.cpp
    TestJob.cpp
    ValidateDBJob.cpp
    LocalServer.cpp
    CursorInfo.cpp
    Server.cpp
    MakefileParser.cpp
    MemoryMonitor.cpp
    GRParser.cpp
    GRTags.cpp
    GccArguments.cpp
    Indexer.cpp
    FileManager.cpp
    Project.cpp
    RTagsClang.cpp
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


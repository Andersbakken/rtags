cmake_minimum_required(VERSION 2.8.3)
include(clang.cmake)

include_directories(
    ${PROJECT_SOURCE_DIR}/3rdparty/leveldb
    )

set(rtags_client_HDRS
    ByteArray.h
    Client.h
    CompilerManager.h
    CompletionMessage.h
    Connection.h
    CreateOutputMessage.h
    Event.h
    EventLoop.h
    EventReceiver.h
    FastDelegate.h
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
    Preprocessor.h
    Process.h
    CompileMessage.h
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
    CompilerManager.cpp
    CompletionMessage.cpp
    Connection.cpp
    CreateOutputMessage.cpp
    EventLoop.cpp
    EventReceiver.cpp
    LocalClient.cpp
    Location.cpp
    Log.cpp
    Messages.cpp
    Path.cpp
    Preprocessor.cpp
    Process.cpp
    CompileMessage.cpp
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
    CompileJob.h
    CompletionJob.h
    CursorInfo.h
    CursorInfoJob.h
    FileManager.h
    FileSystemWatcher.h
    Filter.h
    FindFileJob.h
    FindSymbolsJob.h
    FollowLocationJob.h
    GccArguments.h
    IndexerJob.h
    ListSymbolsJob.h
    LocalServer.h
    Match.h
    MemoryMonitor.h
    Project.h
    RTagsClang.h
    ReferencesJob.h
    ScanJob.h
    Server.h
    StatusJob.h
    ValidateDBJob.h
    )

set(rtags_SRCS
    ${rtags_client_SRCS}
    CompileJob.cpp
    CompletionJob.cpp
    CursorInfoJob.cpp
    FindFileJob.cpp
    FindSymbolsJob.cpp
    FollowLocationJob.cpp
    ScanJob.cpp
    IndexerJob.cpp
    Job.cpp
    ListSymbolsJob.cpp
    ReferencesJob.cpp
    StatusJob.cpp
    ValidateDBJob.cpp
    LocalServer.cpp
    CursorInfo.cpp
    Server.cpp
    MemoryMonitor.cpp
    GccArguments.cpp
    FileManager.cpp
    Project.cpp
    RTagsClang.cpp
   )

set(grtags_SRCS
    GRParser.cpp
    GRTags.cpp
    Location.cpp
    Log.cpp
    Path.cpp
    RTags.cpp
    ReadWriteLock.cpp
)

if(HAVE_INOTIFY EQUAL 1)
  list(APPEND rtags_SRCS FileSystemWatcher_inotify.cpp)
elseif(HAVE_FSEVENTS EQUAL 1)
  list(APPEND rtags_SRCS FileSystemWatcher_fsevents.cpp)
elseif(HAVE_KQUEUE EQUAL 1)
  list(APPEND rtags_SRCS FileSystemWatcher_kqueue.cpp)
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

add_library(rtags ${rtags_SRCS})
add_dependencies(rtags gperf)

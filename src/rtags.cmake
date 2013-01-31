cmake_minimum_required(VERSION 2.8.3)
include(clang.cmake)

include_directories(
    ${PROJECT_SOURCE_DIR}/3rdparty/leveldb
    )

set(rtags_client_SRCS
    Client.cpp
    CompileMessage.cpp
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
    Process.cpp
    QueryMessage.cpp
    RClient.cpp
    RTags.cpp
    ReadWriteLock.cpp
    Semaphore.cpp
    SharedMemory.cpp
    Thread.cpp
    ThreadPool.cpp
    )

set(rtags_SRCS
    ${rtags_client_SRCS}
    CompileJob.cpp
    CompletionJob.cpp
    CursorInfo.cpp
    CursorInfoJob.cpp
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
    MemoryMonitor.cpp
    Preprocessor.cpp
    Project.cpp
    RTagsClang.cpp
    ReferencesJob.cpp
    ScanJob.cpp
    Server.cpp
    StatusJob.cpp
    ValidateDBJob.cpp
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

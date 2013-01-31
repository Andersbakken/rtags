cmake_minimum_required(VERSION 2.8.3)
include(clang.cmake)

include_directories(
    ${PROJECT_SOURCE_DIR}/3rdparty/leveldb
    ${PROJECT_SOURCE_DIR}/3rdparty/rct_install/include
    )

set(rtags_client_SRCS
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
    RTags.cpp
)


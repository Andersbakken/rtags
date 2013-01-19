#ifndef RClient_cpp
#define RClient_cpp

#include "List.h"
#include "ByteArray.h"
#include "Client.h"
class RCCommand;
class Client;
class QueryCommand;
class RClient
{
public:
    RClient();
    ~RClient();
    void exec();
    bool parse(int &argc, char **argv);

    int max() const { return mMax; }
    int logLevel() const { return mLogLevel; }
    int timeout() const { return mTimeout; }

    const Set<ByteArray> &pathFilters() const { return mPathFilters; }
    const Path &cpp() const { return mCpp; }

    const Map<Path, ByteArray> &unsavedFiles() const { return mUnsavedFiles; }

    const List<ByteArray> &rdmArgs() const { return mRdmArgs; }
    const List<ByteArray> &projects() const { return mProjects; }

    ByteArray socketFile() const { return mSocketFile; }

    unsigned queryFlags() const { return mQueryFlags; }
    unsigned clientFlags() const { return mClientFlags; }

    int argc() const { return mArgc; }
    char **argv() const { return mArgv; }
private:
    QueryCommand *addQuery(QueryMessage::Type t, const ByteArray &query = ByteArray());
    void addLog(int level);
    void addCompile(const Path &cwd, const ByteArray &args);

    unsigned mQueryFlags, mClientFlags;
    int mMax, mLogLevel, mTimeout;
    Set<ByteArray> mPathFilters;
    Map<Path, ByteArray> mUnsavedFiles;
    List<RCCommand*> mCommands;
    List<ByteArray> mRdmArgs;
    ByteArray mSocketFile;
    List<ByteArray> mProjects;
    Path mCpp;

    int mArgc;
    char **mArgv;
};

#endif

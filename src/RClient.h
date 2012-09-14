#ifndef RClient_cpp
#define RClient_cpp

#include <List.h>
#include <ByteArray.h>
#include <Client.h>
class RCCommand;
class Client;
class RClient
{
public:
    RClient();
    ~RClient();
    void exec();
    bool parse(int &argc, char **argv);
    unsigned queryFlags() const { return mQueryFlags; }
    int max() const { return mMax; }
    int logLevel() const { return mLogLevel; }
    const Set<ByteArray> &pathFilters() const { return mPathFilters; }
    const Map<Path, ByteArray> &unsavedFiles() const { return mUnsavedFiles; }
    const List<ByteArray> &extraFlags() const { return mExtraFlags; }
    unsigned clientFlags() const { return mClientFlags; }
    List<ByteArray> rdmArgs() const { return mRdmArgs; }
    ByteArray socketFile() const { return mSocketFile; }
private:
    void addQuery(QueryMessage::Type t, const ByteArray &query = ByteArray());
    void addLog(int level);
    void addMakeFile(const Path &makefile, const List<ByteArray> &args);
    void addGRTag(const Path &dir);

    unsigned mQueryFlags;
    int mMax;
    int mLogLevel;
    Set<ByteArray> mPathFilters;
    Map<Path, ByteArray> mUnsavedFiles;
    List<ByteArray> mExtraFlags;
    List<RCCommand*> rCommands;
    unsigned mClientFlags;
    List<ByteArray> mRdmArgs;
    ByteArray mSocketFile;
};

#endif

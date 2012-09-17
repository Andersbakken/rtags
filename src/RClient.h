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

    int max() const { return mMax; }
    int logLevel() const { return mLogLevel; }

    const Set<ByteArray> &pathFilters() const { return mPathFilters; }

    const Map<Path, ByteArray> &unsavedFiles() const { return mUnsavedFiles; }

    const List<ByteArray> &extraFlags() const { return mExtraFlags; }
    const List<ByteArray> &rdmArgs() const { return mRdmArgs; }

    ByteArray socketFile() const { return mSocketFile; }

    unsigned queryFlags() const { return mQueryFlags; }
    unsigned clientFlags() const { return mClientFlags; }
    unsigned makefileFlags() const { return mMakefileFlags; }

    enum MakefileFlag {
        None = 0x0,
        UseDashB = 0x1
    };
private:
    void addQuery(QueryMessage::Type t, const ByteArray &query = ByteArray());
    void addLog(int level);
    void addMakeFile(const Path &makefile, const List<ByteArray> &args);
    void addGRTag(const Path &dir);

    unsigned mQueryFlags, mClientFlags, mMakefileFlags;
    int mMax, mLogLevel;
    Set<ByteArray> mPathFilters;
    Map<Path, ByteArray> mUnsavedFiles;
    List<ByteArray> mExtraFlags;
    List<RCCommand*> rCommands;
    List<ByteArray> mRdmArgs;
    ByteArray mSocketFile;
};

#endif

#ifndef RCCommands_cpp
#define RCCommands_cpp

#include <List.h>
#include <ByteArray.h>
#include <Client.h>
class RCCommand;
class Client;
class RCCommands
{
public:
    RCCommands()
        : queryFlags(0), max(-1), client(0)
    {}
    void addQuery(QueryMessage::Type t, const ByteArray &query = ByteArray());
    void addLog(int level);
    void addMakeFile(const Path &makefile, const List<ByteArray> &args);
    void addGRTag(const Path &dir);
    void exec();

    unsigned queryFlags;
    int max;
    Set<ByteArray> pathFilters;
    Map<Path, ByteArray> unsavedFiles;
    List<ByteArray> extraFlags;
    List<RCCommand*> commands;
    Client *client;

};

#endif

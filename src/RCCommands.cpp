#include "RCCommands.h"
#include "CreateOutputMessage.h"
#include "MakefileMessage.h"
#include "GRTagsMessage.h"

class RCCommand
{
public:
    virtual ~RCCommand() {}
    virtual void exec(RCCommands *commands) = 0;
    virtual ByteArray description() const = 0;
};

class QueryCommand : public RCCommand
{
public:
    QueryCommand(QueryMessage::Type t, const ByteArray &q)
        : type(t), query(q)
    {}

    const QueryMessage::Type type;
    const ByteArray query;

    virtual void exec(RCCommands *commands)
    {
        QueryMessage msg(type, query, commands->queryFlags, commands->max, commands->unsavedFiles);
        msg.setPathFilters(commands->pathFilters.toList());
        commands->client->message(&msg);
    }

    virtual ByteArray description() const
    {
        return ("QueryMessage " + ByteArray::number(type) + " " + query);
    }
};

class RdmLogCommand : public RCCommand
{
public:
    RdmLogCommand(const int &level)
        : mLevel(level)
    {
    }
    virtual void exec(RCCommands *commands)
    {
        CreateOutputMessage msg(mLevel);
        commands->client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return "RdmLogCommand";
    }
    const int &mLevel;
};

class MakefileCommand : public RCCommand
{
public:
    MakefileCommand(const Path &mf, const List<ByteArray> &args)
        : makefile(mf), makefileArgs(args)
    {}
    const Path makefile;
    const List<ByteArray> makefileArgs;
    virtual void exec(RCCommands *commands)
    {
        if (!makefile.isFile()) {
            error() << makefile << "is not a file";
            return;
        }
        MakefileMessage msg(makefile, makefileArgs, commands->extraFlags);
        commands->client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return ("MakefileCommand " + makefile + " " + ByteArray::join(makefileArgs, " "));
    }
};

class GRTagCommand : public RCCommand
{
public:
    GRTagCommand(const Path &dir)
        : directory(dir)
    {}
    const Path directory;
    virtual void exec(RCCommands *commands)
    {
        if (!directory.isDir()) {
            error() << directory << "is not a directory";
            return;
        }
        GRTagsMessage msg(directory);
        commands->client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return ("GRTagMessage " + directory);
    }
};

void RCCommands::addQuery(QueryMessage::Type t, const ByteArray &query)
{
    commands.append(new QueryCommand(t, query));
}
void RCCommands::addLog(int level)
{
    commands.append(new RdmLogCommand(level));
}

void RCCommands::addMakeFile(const Path &makefile, const List<ByteArray> &args)
{
    commands.append(new MakefileCommand(makefile, args));
}

void RCCommands::addGRTag(const Path &dir)
{
    commands.append(new GRTagCommand(dir));
}

void RCCommands::exec()
{
    const int commandCount = commands.size();
    for (int i=0; i<commandCount; ++i) {
        RCCommand *cmd = commands.at(i);
        debug() << "running command " << cmd->description();
        cmd->exec(this);
        delete cmd;
    }
    commands.clear();
}

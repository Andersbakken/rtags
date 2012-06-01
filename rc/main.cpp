#include "Client.h"
#include "QueryMessage.h"
#include "RTags.h"
#include <QByteArray>
#include <QCoreApplication>
#include <QDateTime>
#include <QDebug>
#include <QFile>
#include <QList>
#include <QPair>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <Log.h>

static void help(FILE *f, const char* app)
{
    fprintf(f, "%s options...\n"
            "  --help|-h                                 Display this help\n"
            "  --verbose|-v                              Be more verbose\n"
            "  --skip-paren|-p                           Skip parens in Makefile parsing\n"
            "  --follow-location|-f [arg]                Follow this location\n"
            "  --makefile|-m [arg]                       Process this makefile\n"
            "  --makefile-wait|-M [arg]                  Process this makefile and wait until the whole make process is finished\n"
            "  --reference-name|-R [arg]                 Find references matching arg\n"
            "  --reference-location|-r [arg]             Find references matching this location\n"
            "  --include-declarations-and-definitions|-E Include reference to referenced location\n"
            "  --reverse-sort|-O                         Sort output reversed\n"
            "  --list-symbols|-S [arg]                   List symbol names matching arg\n"
            "  --find-symbols|-F [arg]                   Find symbols matching arg\n"
            "  --dump|-d [arg]                           Dump AST tree of arg \n"
            "  --complete|-c [arg]                       Get code completion for this location\n"
            "  --cursor-info|-C [arg]                    Get cursor info for this location\n"
            "  --unsaved-file|-u [arg]                   Specify an unsaved file and a size to be passed on stdin (e.g. -u main.cpp:343)\n"
            "  --log-file|-L [file]                      Log to this file\n"
            "  --append|-A                               Append to log file\n"
            "  --no-context|-N                           Don't print context for locations\n"
            "  --line-numbers|-l                         Output line numbers instead of offsets\n"
            "  --path-filter|-i [arg]                    Filter out results not matching with arg\n"
            "  --filter-system-headers|-H                Don't exempt system headers from path filters\n"
            "  --includepath|-I [arg]                    Add additional include path, must be combined with --makefile\n"
            "  --define|-D [arg]                         Add additional define, must be combined with --makefile\n"
            "  --compiler-flag|-o [arg]                  Add additional compiler flags, must be combined with --makefile\n"
            "  --test|-t [arg]                           Test whether rtags knows about this source file\n"
            "  --rdm-log|-g                              Receive logs from rdm\n"
            "  --status|-s [arg]                         Dump status of rdm. If arg is passed it should match one of:\n"
            "                                            'general', 'dependencies', 'symbols', 'symbolnames', 'fileinfos' or 'pch'\n"
            "  --name|-N [name]                          Name to use for server (default ~/.rtags/server)"
            "  --autostart-rdm|-a [args]                 Start rdm with [args] if rc fails to connect\n"
            "  --restart-rdm|-e [args]                   Restart rdm with [args] before doing the rest of the commands\n"
            "  --quit-rdm|-q                             Tell server to shut down\n",
            app);
}

static inline QByteArray encodeLocation(const QByteArray &key)
{
    const int lastComma = key.lastIndexOf(',');
    if (lastComma <= 0 || lastComma + 1 >= key.size())
        return QByteArray();

    char *endPtr;
    quint32 offset = strtoull(key.constData() + lastComma + 1, &endPtr, 10);
    if (*endPtr != '\0')
        return QByteArray();
    Path path = Path::resolved(key.left(lastComma));
    QByteArray out;
    {
        QDataStream ds(&out, QIODevice::WriteOnly);
        ds << path << offset;
    }
    return out;
}

struct Command
{
    virtual ~Command() {}
    virtual void exec(Client *client) = 0;
    virtual QByteArray description() const = 0;
};

struct QueryCommand : public Command
{
    QueryCommand(QueryMessage::Type t, const QByteArray &q, const unsigned &qf,
                 const QHash<Path, QByteArray> &u, const QSet<QByteArray> &p)
        : type(t), query(q), queryFlags(qf), unsavedFiles(u), pathFilters(p)
    {}

    const QueryMessage::Type type;
    const QByteArray query;
    const unsigned &queryFlags; // eeh
    const QHash<Path, QByteArray> &unsavedFiles; // eeh
    const QSet<QByteArray> &pathFilters; // eeh

    virtual void exec(Client *client)
    {
        QueryMessage msg(type, query, queryFlags);
        msg.setUnsavedFiles(unsavedFiles);
        msg.setPathFilters(pathFilters.toList());
        client->query(&msg);
    }

    virtual QByteArray description() const
    {
        return ("QueryMessage " + QByteArray::number(type) + " " + query); // ### query might be binary data
    }
};

struct RdmLogCommand : public Command
{
    virtual void exec(Client *client)
    {
        char buf[sizeof(int)];
        int *intPtr = reinterpret_cast<int*>(buf);
        *intPtr = logLevel();
        QueryMessage msg(QueryMessage::RdmLog, QByteArray(buf, sizeof(buf)));
        client->query(&msg);
    }
    virtual QByteArray description() const
    {
        return "RdmLogCommand";
    }
};

struct MakefileCommand : public Command {
    MakefileCommand(const Path &mf, bool w)
        : makefile(mf), wait(w)
    {}
    const Path makefile;
    const bool wait;
    virtual void exec(Client *client)
    {
        if (client->parseMakefile(makefile, wait))
            error("%d source files and %d pch files from %s",
                  client->sourceFileCount(), client->pchCount(), makefile.constData());
    }
    virtual QByteArray description() const
    {
        return ("MakefileCommand " + makefile + (wait ? " wait" : ""));
    }
};

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);

    struct option opts[] = {
        { "verbose", no_argument, 0, 'v' },
        { "skip-paren", no_argument, 0, 'p' },
        { "help", no_argument, 0, 'h' },
        { "autostart-rdm", optional_argument, 0, 'a' },
        { "follow-location", required_argument, 0, 'f' },
        { "makefile", required_argument, 0, 'm' },
        { "makefile-wait", required_argument, 0, 'M' },
        { "reference-name", required_argument, 0, 'R' },
        { "reference-location", required_argument, 0, 'r' },
        { "reverse-sort", no_argument, 0, 'O' },
        { "list-symbols", optional_argument, 0, 'S' },
        { "find-symbols", required_argument, 0, 'F' },
        { "dump", required_argument, 0, 'd' },
        { "complete", required_argument, 0, 'c' },
        { "cursor-info", required_argument, 0, 'C' },
        { "unsaved-file", required_argument, 0, 'u' },
        { "log-file", required_argument, 0, 'L' },
        { "append", no_argument, 0, 'A' },
        { "no-context", no_argument, 0, 'N' },
        { "name", required_argument, 0, 'n' },
        { "status", optional_argument, 0, 's' },
        { "rdm-log", no_argument, 0, 'g' },
        { "line-numbers", no_argument, 0, 'l' },
        { "path-filter", required_argument, 0, 'i' },
        { "filter-system-headers", no_argument, 0, 'H' },
        { "includepath", required_argument, 0, 'I' },
        { "define", required_argument, 0, 'D' },
        { "compiler-flag", required_argument, 0, 'o' },
        { "test", required_argument, 0, 't' },
        { "quit-rdm", no_argument, 0, 'q' },
        { "restart-rdm", optional_argument, 0, 'e' },

        { "include-declarations-and-definitions", no_argument, 0, 'E' },
        { 0, 0, 0, 0 }
    };

    int logLevel = 0;
    QByteArray logFile;
    unsigned logFlags = 0;

    QList<Command*> commands;
    QList<QByteArray> extraFlags;
    QHash<Path, QByteArray> unsavedFiles;
    QSet<QByteArray> pathFilters;
    unsigned queryFlags = 0;
    unsigned clientFlags = 0;
    QList<QByteArray> rdmArgs;
    QByteArray name;

    QFile standardIn;

    const QByteArray shortOptions = RTags::shortOptions(opts);

    for (;;) {
        const int c = getopt_long(argc, argv, shortOptions.constData(), opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 0:
            break;
        case 'h':
            help(stdout, argv[0]);
            return 0;
        case 'n':
            name = optarg;
            break;
        case 'a':
            clientFlags |= Client::AutostartRdm;
            if (optarg)
                rdmArgs = QByteArray::fromRawData(optarg, strlen(optarg)).split(' ');
            break;
        case 'e':
            clientFlags |= Client::RestartRdm;
            if (optarg)
                rdmArgs = QByteArray::fromRawData(optarg, strlen(optarg)).split(' ');
            break;
        case 'E':
            queryFlags |= QueryMessage::IncludeDeclarationsAndDefinitions;
            break;
        case 'O':
            queryFlags |= QueryMessage::ReverseSort;
            break;
        case 'H':
            queryFlags |= QueryMessage::FilterSystemIncludes;
            break;
        case 'I': {
            QByteArray flag("-I");
            flag += optarg;
            extraFlags.append(flag);
            break; }
        case 'D': {
            QByteArray flag("-D");
            flag += optarg;
            extraFlags.append(flag);
            break; }
        case 'o':
            extraFlags.append(optarg);
            break;
        case 'N':
            queryFlags |= QueryMessage::NoContext;
            break;
        case 'i':
            pathFilters.insert(optarg);
            break;
        case 'l':
            queryFlags |= QueryMessage::LineNumbers;
            break;
        case 'v':
            ++logLevel;
            break;
        case 'A':
            logFlags |= Append;
            break;
        case 'L':
            logFile = optarg;
            break;
        case 'p':
            queryFlags |= QueryMessage::SkipParentheses;
            break;
        case 'u':
            if (!standardIn.isOpen() && !standardIn.open(stdin, QIODevice::ReadOnly)) {
                qWarning("Can't open stdin for reading");
                return 1;
            } else {
                const QByteArray arg(optarg);
                const int colon = arg.lastIndexOf(':');
                if (colon == -1) {
                    qWarning("Can't parse -u [%s]", optarg);
                    return 1;
                }
                const int bytes = atoi(arg.constData() + colon + 1);
                if (!bytes) {
                    qWarning("Can't parse -u [%s]", optarg);
                    return 1;
                }
                const QByteArray contents = standardIn.read(bytes);
                unsavedFiles[Path::resolved(arg.left(colon))] = contents;
            }
            break;
        case 'f':
        case 'C':
        case 'r': {
            QByteArray encoded = encodeLocation(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s", optarg);
                return 1;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'f': type = QueryMessage::FollowLocation; break;
            case 'C': type = QueryMessage::CursorInfo; break;
            case 'r': type = QueryMessage::ReferencesLocation; break;
            }
            commands.append(new QueryCommand(type, encoded, queryFlags, unsavedFiles, pathFilters)); // these are references
            break; }
        case 'g':
            commands.append(new RdmLogCommand);
            break;
        case 'q':
            commands.append(new QueryCommand(QueryMessage::Shutdown, QByteArray(), queryFlags, unsavedFiles, pathFilters)); // these are references
            break;
        case 't':
            commands.append(new QueryCommand(QueryMessage::Test, Path::resolved(optarg), queryFlags, unsavedFiles, pathFilters)); // these are references
            break;
        case 'm':
            commands.append(new MakefileCommand(Path::resolved(optarg), false));
            break;
        case 'M':
            commands.append(new MakefileCommand(Path::resolved(optarg), true));
            break;
        case 's':
            if (optarg) {
                commands.append(new QueryCommand(QueryMessage::Status, optarg, queryFlags, unsavedFiles, pathFilters)); // these are references
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(QueryMessage::Status, argv[optind++], queryFlags, unsavedFiles, pathFilters)); // these are references
            } else {
                commands.append(new QueryCommand(QueryMessage::Status, QByteArray(), queryFlags, unsavedFiles, pathFilters)); // these are references
            }
            break;
        case 'R':
            commands.append(new QueryCommand(QueryMessage::ReferencesName, optarg, queryFlags, unsavedFiles, pathFilters)); // these are references
            break;
        case 'S':
            if (optarg) {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, optarg, queryFlags, unsavedFiles, pathFilters)); // these are references
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, argv[optind++], queryFlags, unsavedFiles, pathFilters)); // these are references
            } else {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, QByteArray(), queryFlags, unsavedFiles, pathFilters)); // these are references
            }
            break;
        case 'F':
            commands.append(new QueryCommand(QueryMessage::FindSymbols, optarg, queryFlags, unsavedFiles, pathFilters)); // these are references
            break;
        case 'd':
            commands.append(new QueryCommand(QueryMessage::Dump, Path::resolved(optarg), queryFlags, unsavedFiles, pathFilters)); // these are references
            break;
        case '?':
            // getopt printed an error message already
            break;
        default:
            break;
        }
    }
    if (optind < argc) {
        fprintf(stderr, "rc: unexpected option -- '%s'\n", argv[optind]);
        return 1;
    }

    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile.constData(), logFlags);
        return 1;
    }

    if (commands.isEmpty() && !(clientFlags & (Client::RestartRdm|Client::AutostartRdm))) {
        help(stderr, argv[0]);
        return 1;
    }

    if (!logFile.isEmpty() || logLevel > 0) {
        Log l(1);
        l << argc;
        for (int i = 0; i < argc; ++i)
            l << argv[i];
    }

    if (name.isEmpty())
        name = RTags::rtagsDir() + "server";

    Client client(name, clientFlags, extraFlags, rdmArgs);
    foreach(Command *cmd, commands) {
        debug() << "running command" << cmd->description();
        cmd->exec(&client);
        delete cmd;
    }
    return 0;
}

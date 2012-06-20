#include "Client.h"
#include "QueryMessage.h"
#include "OutputMessage.h"
#include "MakefileMessage.h"
#include "RTags.h"
#include <ByteArray.h>
#include <QCoreApplication>
#include <QDateTime>
#include <QDebug>
#include <QFile>
#include <List.h>
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
            "  --elisp-list|-P                           Output elisp: (list \"one\" \"two\" ...)\n"
            "  --follow-location|-f [arg]                Follow this location\n"
            "  --makefile|-m [arg]                       Process this makefile\n"
            "  --remake|-M [optional regexp]             Remake makefiles matching regexp or all if no regexp\n"
            "  --reference-name|-R [arg]                 Find references matching arg\n"
            "  --reference-location|-r [arg]             Find references matching this location\n"
            "  --include-declarations-and-definitions|-E Include reference to referenced location\n"
            "  --reverse-sort|-O                         Sort output reversed\n"
            "  --list-symbols|-S [arg]                   List symbol names matching arg\n"
            "  --find-symbols|-F [arg]                   Find symbols matching arg\n"
            "  --dump|-d [arg]                           Dump AST tree of arg \n"
            "  --complete|-c [arg]                       Get code completion for this location\n"
            "  --cursor-info|-U [arg]                    Get cursor info for this location\n"
            "  --log-file|-L [file]                      Log to this file\n"
            "  --append|-A                               Append to log file\n"
            "  --no-context|-N                           Don't print context for locations\n"
            "  --line-numbers|-l                         Output line numbers instead of offsets\n"
            "  --path-filter|-i [arg]                    Filter out results not matching with arg\n"
            "  --same-file|-z                            Filter out results not in the same file\n"
            "  --filter-system-headers|-H                Don't exempt system headers from path filters\n"
            "  --includepath|-I [arg]                    Add additional include path, must be combined with --makefile\n"
            "  --define|-D [arg]                         Add additional define, must be combined with --makefile\n"
            "  --compiler-flag|-o [arg]                  Add additional compiler flags, must be combined with --makefile\n"
            "  --test|-t [arg]                           Test whether rtags knows about this source file\n"
            "  --fixits|-x [file]                        Get fixits for file\n"
            "  --errors|-Q [file]                        Get errors for file\n"
            "  --rdm-log|-g                              Receive logs from rdm\n"
            "  --status|-s [arg]                         Dump status of rdm. If arg is passed it should match one of:\n"
            "                                            'general', 'dependencies', 'symbols', 'symbolnames', 'fileinfos' or 'pch'\n"
            "  --name|-n [name]                          Name to use for server (default ~/.rtags/server)\n"
            "  --autostart-rdm|-a [args]                 Start rdm with [args] if rc fails to connect\n"
            "  --restart-rdm|-e [args]                   Restart rdm with [args] before doing the rest of the commands\n"
            "  --run-test|-T [file]                      Run tests from file\n"
            "  --diagnostics|-G                          Open a connection that prints diagnostics\n"
            "  --clear-db|-C                             Clear database, use with care\n"
            "  --reindex|-V [optional regexp]            Reindex all files or all files matching pattern\n"
            "  --quit-rdm|-q                             Tell server to shut down\n",
            app);
}

static inline ByteArray encodeLocation(const ByteArray &key)
{
    const int lastComma = key.lastIndexOf(',');
    if (lastComma <= 0 || lastComma + 1 >= key.size())
        return ByteArray();

    char *endPtr;
    uint32_t offset = strtoull(key.constData() + lastComma + 1, &endPtr, 10);
    if (*endPtr != '\0')
        return ByteArray();
    Path path = Path::resolved(key.left(lastComma));
    ByteArray out;
    {
        out = path;
        char buf[4];
        memcpy(buf, &offset, sizeof(buf));
        out += ByteArray(buf, 4);
    }

    return out;
}

struct Command
{
    virtual ~Command() {}
    virtual void exec(Client *client) = 0;
    virtual ByteArray description() const = 0;
};

struct QueryCommand : public Command
{
    QueryCommand(QueryMessage::Type t, const ByteArray &q, const unsigned &qf, const Set<ByteArray> &p)
        : type(t), query(q), queryFlags(qf), pathFilters(p)
    {}

    const QueryMessage::Type type;
    const ByteArray query;
    const unsigned &queryFlags; // eeh
    const Set<ByteArray> &pathFilters; // eeh

    virtual void exec(Client *client)
    {
        QueryMessage msg(type, query, queryFlags);
        msg.setPathFilters(pathFilters.toList());
        client->message(&msg);
    }

    virtual ByteArray description() const
    {
        return ("QueryMessage " + ByteArray::number(type) + " " + query); // ### query might be binary data
    }
};

struct RdmLogCommand : public Command
{
    RdmLogCommand(const ByteArray &cmd)
        : mCmd(cmd)
    {
    }
    virtual void exec(Client *client)
    {
        OutputMessage msg(mCmd, logLevel());
        client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return "RdmLogCommand";
    }
private:
    ByteArray mCmd;
};

struct MakefileCommand : public Command
{
    MakefileCommand(const Path &mf, const List<ByteArray> &args, const List<ByteArray> &ef)
        : makefile(mf), makefileArgs(args), extraFlags(ef)
    {}
    const Path makefile;
    const List<ByteArray> makefileArgs;
    const List<ByteArray> &extraFlags; // reference
    virtual void exec(Client *client)
    {
        if (!makefile.isFile()) {
            error() << makefile << "is not a file";
            return;
        }
        MakefileMessage msg(makefile, makefileArgs, extraFlags);
        client->message(&msg);
    }
    virtual ByteArray description() const
    {
        return ("MakefileCommand " + makefile + " " + RTags::join(makefileArgs, " ") + " " + RTags::join(extraFlags, " "));
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
        { "remake", optional_argument, 0, 'M' },
        { "reference-name", required_argument, 0, 'R' },
        { "reference-location", required_argument, 0, 'r' },
        { "reverse-sort", no_argument, 0, 'O' },
        { "list-symbols", optional_argument, 0, 'S' },
        { "find-symbols", required_argument, 0, 'F' },
        { "dump", required_argument, 0, 'd' },
        { "complete", required_argument, 0, 'c' },
        { "cursor-info", required_argument, 0, 'U' },
        { "log-file", required_argument, 0, 'L' },
        { "append", no_argument, 0, 'A' },
        { "no-context", no_argument, 0, 'N' },
        { "name", required_argument, 0, 'n' },
        { "status", optional_argument, 0, 's' },
        { "rdm-log", no_argument, 0, 'g' },
        { "line-numbers", no_argument, 0, 'l' },
        { "path-filter", required_argument, 0, 'i' },
        { "same-file", no_argument, 0, 'z' },
        { "filter-system-headers", no_argument, 0, 'H' },
        { "includepath", required_argument, 0, 'I' },
        { "define", required_argument, 0, 'D' },
        { "compiler-flag", required_argument, 0, 'o' },
        { "test", required_argument, 0, 't' },
        { "quit-rdm", no_argument, 0, 'q' },
        { "restart-rdm", optional_argument, 0, 'e' },
        { "include-declarations-and-definitions", no_argument, 0, 'E' },
        { "elisp-list", no_argument, 0, 'P' },
        { "run-test", required_argument, 0, 'T' },
        { "clear-db", no_argument, 0, 'C' },
        { "fixits", required_argument, 0, 'x' },
        { "errors", required_argument, 0, 'Q' },
        { "reindex", optional_argument, 0, 'V' },
        { "diagnostics", no_argument, 0, 'G' },
        { 0, 0, 0, 0 }
    };

    // Not taken: b j k w y

    int logLevel = 0;
    ByteArray logFile;
    unsigned logFlags = 0;

    List<Command*> commands;
    List<ByteArray> extraFlags;
    Set<ByteArray> pathFilters;
    unsigned queryFlags = 0;
    unsigned clientFlags = 0;
    List<ByteArray> rdmArgs;
    ByteArray name;

    QFile standardIn;

    const ByteArray shortOptions = RTags::shortOptions(opts);

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
                rdmArgs = ByteArray(optarg, strlen(optarg)).split(' ');
            break;
        case 'e':
            clientFlags |= Client::RestartRdm;
            if (optarg)
                rdmArgs = ByteArray(optarg, strlen(optarg)).split(' ');
            break;
        case 'E':
            queryFlags |= QueryMessage::IncludeDeclarationsAndDefinitions;
            break;
        case 'z':
            queryFlags |= QueryMessage::SameFile;
            break;
        case 'O':
            queryFlags |= QueryMessage::ReverseSort;
            break;
        case 'P':
            queryFlags |= QueryMessage::ElispList;
            break;
        case 'H':
            queryFlags |= QueryMessage::FilterSystemIncludes;
            break;
        case 'I': {
            ByteArray flag("-I");
            flag += optarg;
            extraFlags.append(flag);
            break; }
        case 'D': {
            ByteArray flag("-D");
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
        case 'f':
        case 'U':
        case 'r': {
            const ByteArray encoded = encodeLocation(optarg);
            if (encoded.isEmpty()) {
                fprintf(stderr, "Can't resolve argument %s\n", optarg);
                return 1;
            }
            QueryMessage::Type type = QueryMessage::Invalid;
            switch (c) {
            case 'f': type = QueryMessage::FollowLocation; break;
            case 'U': type = QueryMessage::CursorInfo; break;
            case 'r': type = QueryMessage::ReferencesLocation; break;
            }
            commands.append(new QueryCommand(type, encoded, queryFlags, pathFilters));
            break; }
        case 'C':
            commands.append(new QueryCommand(QueryMessage::ClearDatabase, ByteArray(), queryFlags, pathFilters));
            break;
        case 'g':
            commands.append(new RdmLogCommand("log"));
            break;
        case 'G':
            commands.append(new RdmLogCommand("CError"));
            break;
        case 'q':
            commands.append(new QueryCommand(QueryMessage::Shutdown, ByteArray(), queryFlags, pathFilters));
            break;
        case 'V':
        case 'M': {
            const QueryMessage::Type type = (c == 'V' ? QueryMessage::Reindex : QueryMessage::Remake);
            if (optarg) {
                commands.append(new QueryCommand(type, optarg, queryFlags, pathFilters));
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(type, argv[optind++], queryFlags, pathFilters));
            } else {
                commands.append(new QueryCommand(type, ByteArray(), queryFlags, pathFilters));
            }
            break; }
        case 't':
        case 'x':
        case 'Q':
        case 'd':
        case 'T': {
            const Path p = Path::resolved(optarg);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return 1;
            }
            QueryMessage::Type type;
            switch (c) {
            case 't': type = QueryMessage::Test; break;
            case 'x': type = QueryMessage::FixIts; break;
            case 'Q': type = QueryMessage::Errors; break;
            case 'T': type = QueryMessage::RunTest; break;
            case 'd': type = QueryMessage::Dump; break;
            }

            commands.append(new QueryCommand(type, p, queryFlags, pathFilters));
            break; }
        case 'm': {
            const Path p = Path::resolved(optarg);
            if (!p.isFile()) {
                fprintf(stderr, "%s is not a file\n", optarg);
                return 1;
            }

            List<ByteArray> makefileArgs;
            while (optind < argc && argv[optind][0] != '-')
                makefileArgs.append(argv[optind++]);
            commands.append(new MakefileCommand(p, makefileArgs, extraFlags));
            break; }
        case 's':
            if (optarg) {
                commands.append(new QueryCommand(QueryMessage::Status, optarg, queryFlags, pathFilters));
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(QueryMessage::Status, argv[optind++], queryFlags, pathFilters));
            } else {
                commands.append(new QueryCommand(QueryMessage::Status, ByteArray(), queryFlags, pathFilters));
            }
            break;
        case 'R':
            commands.append(new QueryCommand(QueryMessage::ReferencesName, optarg, queryFlags, pathFilters));
            break;
        case 'S':
            if (optarg) {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, optarg, queryFlags, pathFilters));
            } else if (optind < argc && argv[optind][0] != '-') {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, argv[optind++], queryFlags, pathFilters));
            } else {
                commands.append(new QueryCommand(QueryMessage::ListSymbols, ByteArray(), queryFlags, pathFilters));
            }
            break;
        case 'F':
            commands.append(new QueryCommand(QueryMessage::FindSymbols, optarg, queryFlags, pathFilters));
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

    Client client(name, clientFlags, rdmArgs);
    foreach(Command *cmd, commands) {
        debug() << "running command" << cmd->description();
        cmd->exec(&client);
        delete cmd;
    }
    return 0;
}

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
            "  --help|-h                     Display this help\n"
            "  --verbose|-v                  Be more verbose\n"
            "  --skip-paren|-p               Skip parens in Makefile parsing\n"
            "  --follow-location|-f [arg]    Follow this location\n"
            "  --makefile|-m [arg]           Process this makefile\n"
            "  --reference-name|-n [arg]     Find references matching arg\n"
            "  --reference-location|-l [arg] Find references matching this location\n"
            "  --recompile|-r [arg]          Recompile this source file\n"
            "  --match|-a [arg]              Find symbol matching arg\n"
            "  --dump|-d [arg]               Dump AST tree of arg \n"
            "  --complete|-c [arg]           Get code completion for this location\n"
            "  --cursor-info|-C [arg]        Get cursor info for this location\n"
            "  --unsaved-file|-u [arg]       Specify an unsaved file and a size to be passed on stdin (e.g. -u main.cpp:343)\n"
            "  --log-file|-L [file]          Log to this file\n"
            "  --append|-A                   Append to log file\n"
            "  --no-context|-N               Don't print context for locations\n"
            "  --poke|-P                     Poke file\n"
            "  --status|-s                   Dump status of rdm\n",
            app);
}

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);

    struct option opts[] = {
        { "verbose", no_argument, 0, 'v' },
        { "skip-paren", no_argument, 0, 'p' },
        { "help", no_argument, 0, 'h' },
        { "follow-location", required_argument, 0, 'f' },
        { "makefile", required_argument, 0, 'm' },
        { "reference-name", required_argument, 0, 'n' },
        { "reference-location", required_argument, 0, 'l' },
        { "recompile", required_argument, 0, 'r' },
        { "match", required_argument, 0, 'a' },
        { "dump", required_argument, 0, 'd' },
        { "complete", required_argument, 0, 'c' },
        { "cursor-info", required_argument, 0, 'C' },
        { "unsaved-file", required_argument, 0, 'u' },
        { "log-file", required_argument, 0, 'L' },
        { "append", no_argument, 0, 'A' },
        { "no-context", no_argument, 0, 'N' },
        { "status", no_argument, 0, 's' },
        { "poke", required_argument, 0, 'P' },
        { 0, 0, 0, 0 }
    };

    int logLevel = 0;
    QByteArray logFile;
    unsigned logFlags = 0;

    QList<Path> makeFiles;
    QList<QPair<QueryMessage::Type, QByteArray> > optlist;
    QHash<Path, QByteArray> unsavedFiles;
    unsigned flags = 0;

    QFile standardIn;

    for (;;) {
        const int c = getopt_long(argc, argv, "vphf:m:n:l:r:a:d:c:C:u:L:ANsP:", opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 0:
            break;
        case 'h':
            help(stdout, argv[0]);
            return 0;
        case 'N':
            flags |= Client::NoContext;
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
            flags |= Client::SkipParen;
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
        case 'l': {
            QByteArray resolved;
            RTags::Location loc;
            if (!RTags::makeLocation(optarg, &loc, &resolved)) {
                qWarning("Can't resolve argument %s", optarg);
                return 1;
            }
            optlist.append(qMakePair((c == 'l'
                                      ? QueryMessage::ReferencesLocation
                                      : QueryMessage::FollowLocation), resolved));
            break; }
        case 'm':
            makeFiles.append(Path::resolved(optarg));
            break;
        case 's':
            optlist.append(qMakePair(QueryMessage::Status, QByteArray()));
            break;
        case 'n':
            optlist.append(qMakePair(QueryMessage::ReferencesName, QByteArray(optarg)));
            break;
        case 'r':
            optlist.append(qMakePair<QueryMessage::Type, QByteArray>(QueryMessage::Recompile, Path::resolved(optarg)));
            break;
        case 'P':
            optlist.append(qMakePair<QueryMessage::Type, QByteArray>(QueryMessage::Poke, Path::resolved(optarg)));
            break;
        case 'a':
            optlist.append(qMakePair(QueryMessage::Match, QByteArray(optarg)));
            break;
        case 'd':
            optlist.append(qMakePair<QueryMessage::Type, QByteArray>(QueryMessage::Dump, Path::resolved(optarg)));
            break;
        case '?':
            // getopt printed an error message already
        default:
            break;
        }
    }

    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile.constData(), logFlags);
        return 1;
    }

    if (optlist.isEmpty() && makeFiles.isEmpty()) {
        help(stderr, argv[0]);
        return 1;
    }

    if (!logFile.isEmpty() || logLevel > 0) {
        Log l(1);
        l << argc;
        for (int i = 0; i < argc; ++i)
            l << argv[i];
    }

    Client client(flags);
    QList<QPair<QueryMessage::Type, QByteArray> >::const_iterator it = optlist.begin();
    while (it != optlist.end()) {
        client.query(it->first, it->second, unsavedFiles);
        ++it;
    }
    foreach(const QByteArray &makeFile, makeFiles) {
        client.parseMakefile(makeFile);
    }
    return 0;
}

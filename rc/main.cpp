#include "Client.h"
#include "QueryMessage.h"
#include <QCoreApplication>
#include <QList>
#include <QPair>
#include <QByteArray>
#include <QDateTime>
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>

static int help(const char* app)
{
    fprintf(stderr, "%s [-v] [-e] [-m Makefile] [-f follow-location] [-n reference-name] "
                    "[-l reference-location] [-r filename]\n", app);
    return 1;
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
        { 0, 0, 0, 0 }
    };

    bool verbose = false;
    bool skipparen = false;
    QList<Path> makeFiles;
    QList<QPair<QueryMessage::Type, QByteArray> > optlist;

    if (getenv("LOG_RC")) {
        FILE* logfile = fopen("/tmp/rc.log", "a");
        if (logfile) {
            QDateTime time = QDateTime::currentDateTime();
            fprintf(logfile, "%s (%d): ", qPrintable(time.toString()), argc);
            for (int i = 0; i < argc; ++i)
                fprintf(logfile, "\"%s\" ", argv[i]);
            fprintf(logfile, "\name");
            fclose(logfile);
        }
    }

    int idx, c;
    for (;;) {
        c = getopt_long(argc, argv, "vphf:m:n:l:r:a:d:c:", opts, &idx);
        if (c == -1)
            break;
        switch (c) {
        case 0:
            printf("long? %d\n", idx);
            break;
        case 'h':
            return help(argv[0]);
        case 'v':
            verbose = true;
            break;
        case 'p':
            skipparen = true;
            break;
        case 'f':
            optlist.append(qMakePair(QueryMessage::FollowLocation, QByteArray(optarg)));
            break;
        case 'c':
            optlist.append(qMakePair(QueryMessage::CodeComplete, QByteArray(optarg)));
            break;
        case 'm':
            makeFiles.append(Path::resolved(optarg));
            break;
        case 'n':
            optlist.append(qMakePair(QueryMessage::ReferencesName, QByteArray(optarg)));
            break;
        case 'l':
            optlist.append(qMakePair(QueryMessage::ReferencesLocation, QByteArray(optarg)));
            break;
        case 'r':
            optlist.append(qMakePair<QueryMessage::Type, QByteArray>(QueryMessage::Recompile, Path::resolved(optarg)));
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

    if (optlist.isEmpty() && makeFiles.isEmpty())
        return help(argv[0]);

    int flags = 0;
    if (verbose)
        flags |= Client::Verbose;
    if (skipparen)
        flags |= Client::SkipParen;
    Client client(flags);
    QList<QPair<QueryMessage::Type, QByteArray> >::const_iterator it = optlist.begin();
    while (it != optlist.end()) {
        client.query(it->first, it->second);
        ++it;
    }
    foreach(const QByteArray &makeFile, makeFiles) {
        client.parseMakefile(makeFile);
    }
    return 0;
}

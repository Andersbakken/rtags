#include "Client.h"
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
    fprintf(stderr, "%s [-m Makefile] [-f follow-location] [-n reference-name] "
                    "[-l reference-location] [-r filename]\n", app);
    return 1;
}

enum OptType { FollowLocation, Makefile, ReferenceName, ReferenceLocation, Recompile, Match };

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);

    struct option opts[] = {
        { "verbose", no_argument, 0, 'v' },
        { "help", no_argument, 0, 'h' },
        { "follow-location", required_argument, 0, 'f' },
        { "makefile", required_argument, 0, 'm' },
        { "reference-name", required_argument, 0, 'n' },
        { "reference-location", required_argument, 0, 'l' },
        { "recompile", required_argument, 0, 'r' },
        { "match", required_argument, 0, 'a' },
        { 0, 0, 0, 0 }
    };

    bool verbose = false;
    QList<QPair<OptType, QByteArray> > optlist;

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
        c = getopt_long(argc, argv, "vhf:m:n:l:r:a:", opts, &idx);
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
        case 'f':
            optlist.append(qMakePair(FollowLocation, QByteArray(optarg)));
            break;
        case 'm':
            optlist.append(qMakePair(Makefile, QByteArray(optarg)));
            break;
        case 'n':
            optlist.append(qMakePair(ReferenceName, QByteArray(optarg)));
            break;
        case 'l':
            optlist.append(qMakePair(ReferenceLocation, QByteArray(optarg)));
            break;
        case 'r':
            optlist.append(qMakePair(Recompile, QByteArray(optarg)));
            break;
        case 'a':
            optlist.append(qMakePair(Match, QByteArray(optarg)));
            break;
        case '?':
            // getopt printed an error message already
        default:
            break;
        }
    }

    if (optlist.isEmpty())
        return help(argv[0]);

    Client client(verbose ? Client::Verbose : Client::Silent);
    QList<QPair<OptType, QByteArray> >::const_iterator it = optlist.begin();
    while (it != optlist.end()) {
        switch (it->first) {
        case FollowLocation:
            client.query(Client::FollowLocation, it->second);
            break;
        case Makefile:
            client.parseMakefile(it->second);
            break;
        case ReferenceName:
            client.query(Client::ReferencesName, it->second);
            break;
        case ReferenceLocation:
            client.query(Client::ReferencesLocation, it->second);
            break;
        case Recompile:
            client.query(Client::Recompile, it->second);
            break;
        case Match:
            client.query(Client::Match, it->second);
            break;
        }
        ++it;
    }
    return 0;
}

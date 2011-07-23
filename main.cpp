#include <QCoreApplication>
#include <QString>
#include <stdio.h>
#include "Daemon.h"
#include "Client.h"
#include <syslog.h>

#define CLIENT_CONNECT_ATTEMPTS 5
#define CLIENT_CONNECT_DELAY 1

static QStringList buildArgs(const QStringList& args, const QString& cmd)
{
    QStringList ret = args;
    if (ret.size() == 1)
        ret << cmd;
    else if (ret.size() > 1)
        ret[1] = cmd;
    return ret;
}

void syslogMsgHandler(QtMsgType t, const char* str)
{
    int priority = LOG_WARNING;
    static const char *names[] = { "DEBUG", "WARNING", "CRITICAL", "FATAL" };
    switch (t) {
    case QtDebugMsg:
        priority = LOG_DEBUG;
        break;
    case QtWarningMsg:
        priority = LOG_WARNING;
        break;
    case QtCriticalMsg:
        priority = LOG_CRIT;
        break;
    case QtFatalMsg:
        priority = LOG_CRIT;
        break;
    }
    fprintf(stderr, "%s: %s\n", names[t], str);
    syslog(priority, "%s\n", str);
}


int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("RTags");
    QCoreApplication::setApplicationName("rtags");

    QByteArray cmd;
    if (argc == 1) {
        cmd = "syntax";
    } else {
        cmd = argv[1];
    }

    if (cmd == "daemonize") {
        Daemon daemon;
        qInstallMsgHandler(syslogMsgHandler);
        if (daemon.start())
            return app.exec();
        else
            return -2;
    } else {
        Client client;
        if (!client.connect()) {
            if (cmd == "quit")
                return 0;
            client.startDaemon(app.arguments());
            for (int i = 0; i < CLIENT_CONNECT_ATTEMPTS; ++i) {
                if (client.connect()) {
                    break;
                }
                sleep(CLIENT_CONNECT_DELAY);
            }
        }
        for (int i = 0; i < CLIENT_CONNECT_ATTEMPTS; ++i) {
            if (client.connected()) {
                QString reply = client.exec(buildArgs(app.arguments(), cmd));
                if (!reply.isEmpty())
                    printf("%s\n", qPrintable(reply));
                return 0;
            }
            sleep(CLIENT_CONNECT_DELAY);
        }
    }
    qWarning("Couldn't connect to daemon");

    return -1;
}

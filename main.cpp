#include <QCoreApplication>
#include <QString>
#include <stdio.h>
#include "Daemon.h"
#include "Client.h"
#include "Utils.h"
#include <syslog.h>

#define CLIENT_CONNECT_ATTEMPTS 5
#define CLIENT_CONNECT_DELAY 1

static QStringList buildArgs(const QStringList& args, const QString& cmd)
{
    FUNC;
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
    fprintf(stderr, "%s (%s)\n", str, names[t]);
    QFile file("/tmp/rtags.log");
    file.open(QIODevice::WriteOnly|QIODevice::Append);
    char buf[1024];
    const int s = snprintf(buf, 1023, "%s (%s)\n", str, names[t]);
    file.write(buf, s);
    syslog(priority, "%s (%s)\n", str, names[t]);
}


int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);
    QThread::currentThread()->setObjectName("main");
    QStringList args = app.arguments();
    if (args.contains("--verbose")) {
        Options::s_verbose = true;
        args.removeAll("--verbose");
    }
    if (args.contains("-v")) {
        args.removeAll("-v");
        Options::s_verbose = true;
    }

    if (args.size() == 1)
        args.append("syntax");

    const QString &cmd = args[1];
    FUNC;
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("RTags");
    QCoreApplication::setApplicationName("rtags");


    if (cmd == QLatin1String("daemonize")) {
        Daemon daemon;
        qInstallMsgHandler(syslogMsgHandler);
        if (daemon.start())
            return app.exec();
        else
            return -2;
    } else {
        Client client;
        if (!client.connect()) {
            if (cmd == QLatin1String("quit"))
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

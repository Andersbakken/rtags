#include <QCoreApplication>
#include <QString>
#include <QList>
#include <stdio.h>
#include "Daemon.h"
#include "Client.h"
#include "Utils.h"
#include <syslog.h>
#include "ArgParser.h"


void syslogMsgHandler(QtMsgType t, const char* str)
{
    int priority = LOG_WARNING;
    static const char *names[] = { "DEBUG", "WARNING", "CRITICAL", "FATAL" };
    const bool noColors = getenv("RTAGS_CONSOLE_NO_COLOR");
    const char *colorStart = "";
    const char *colorEnd = "";

    switch (t) {
    case QtDebugMsg:
        colorStart = "\x1b[36m"; // cyan
        priority = LOG_DEBUG;
        break;
    case QtWarningMsg:
        colorStart = "\x1b[31m"; // red
        priority = LOG_WARNING;
        break;
    case QtCriticalMsg:
        colorStart = "\x1b[31m";
        priority = LOG_CRIT;
        break;
    case QtFatalMsg:
        colorStart = "\x1b[41;37m";
        priority = LOG_CRIT;
        break;
    }
    if (noColors) {
        colorStart = "";
    } else if (colorStart) {
        colorEnd = "\x1b[0m";
    }
    fprintf(stderr, "%s%s (%s): %s%s\n", colorStart,
            qPrintable(QDateTime::currentDateTime().toString()),
            qPrintable(QThread::currentThread()->objectName()),
            str, colorEnd);
    QFile file("/tmp/rtags.log");
    file.open(QIODevice::WriteOnly|QIODevice::Append);
    char buf[16384];
    const int s = snprintf(buf, 16383, "%s (%s): %s (%s)\n",
                           qPrintable(QDateTime::currentDateTime().toString()),
                           qPrintable(QThread::currentThread()->objectName()),
                           str,
                           names[t]);
    file.write(buf, s);
    syslog(priority, "%s (%s)\n", str, names[t]);
}

int main(int argc, char** argv)
{
    unsetenv("MAKEFLAGS");
    if (QFile::exists("/tmp/rtags.log")) {
        int idx = 1;
        while (QFile::exists(QString("/tmp/rtags.log.%1").arg(idx)))
            ++idx;
        QFile::rename("/tmp/rtags.log", QString("/tmp/rtags.log.%1").arg(idx));
    }
    QCoreApplication app(argc, argv);
    QThread::currentThread()->setObjectName("main");
    qRegisterMetaType<QList<QByteArray> >();
    qRegisterMetaType<ByteArrayHash>();
    qRegisterMetaTypeStreamOperators<QList<QByteArray> >("QList<QByteArray>");
    qRegisterMetaTypeStreamOperators<ByteArrayHash>("ByteArrayHash");
    ArgParser args(argc, argv);
    QHash<QByteArray, QVariant> argsmap = args.dashArguments();
    if (argsmap.contains("verbose")) {
        Options::s_verbose = true;
        argsmap.remove("verbose");
    }
    if (argsmap.contains("v")) {
        argsmap.remove("v");
        Options::s_verbose = true;
    }

    bool ok;
    int timeout = argsmap.value("timeout").toUInt(&ok);
    if (!ok)
        timeout = 1000;

    if (argsmap.isEmpty())
        argsmap.insert("command", "daemonize");

    QByteArray cmd = argsmap.value("command").toByteArray();
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("rtags");
    QCoreApplication::setApplicationName("rtags");

    if (Options::s_verbose)
        qDebug() << argsmap;

    if (cmd == "daemonize") {
        {
            Client client;
            if (client.connect(timeout)) {
                QHash<QByteArray, QVariant> args;
                args["command"] = "quit";
                client.exec(args, QList<QByteArray>());
                usleep(1000); // wait for other daemon to die?
            }
        }

        qInstallMsgHandler(syslogMsgHandler);
        Daemon daemon;
        if (daemon.start()) {
            return app.exec();
        } else {
            return -2;
        }
    } else {
        Client client;
        if (!client.connect(timeout)) {
            if (cmd == "quit") {
                qWarning("Can't connect to rtags daemon");
                return 0;
            }
            if (!getenv("RTAGS_NO_AUTOSTART") || argsmap.contains("autostart")) {
                client.startDaemon(app.arguments());
                sleep(1); // ### hmmmm
                if (!client.connect(timeout)) {
                    qWarning("Can't connect to rtags daemon");
                    return 1;
                }
            }
        }
        if (client.isConnected()) {
            QHash<QByteArray, QVariant> replymap = client.exec(argsmap, args.freeArguments());
            QByteArray reply = replymap.value("result").toByteArray();
            if (!reply.isEmpty())
                printf("%s\n", reply.constData());
            return 0;
        }
    }
    qWarning("Couldn't connect to daemon");
    return -1;
}

#include <QCoreApplication>
#include <QString>
#include <QList>
#include <stdio.h>
#include "Daemon.h"
#include "Client.h"
#include "Utils.h"
#include <syslog.h>
#include "ArgParser.h"
#include "TemporaryFiles.h"

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
    char buf[16384];
    const int s = snprintf(buf, 16383, "%s (%s): %s (%s)\n",
                           qPrintable(QDateTime::currentDateTime().toString()),
                           qPrintable(QThread::currentThread()->objectName()),
                           str,
                           names[t]);
    
    static QMutex sFileLock;
    {
        QMutexLocker lock(&sFileLock);
        QFile file("/tmp/rtags.log");
        file.open(QIODevice::WriteOnly|QIODevice::Append);
        file.write(buf, s);
    }
    syslog(priority, "%s (%s)\n", str, names[t]);
}

int main(int argc, char** argv)
{
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

    QList<QByteArray> freeArgs = args.freeArguments();
    if (freeArgs.isEmpty())
        freeArgs.append("daemonize");

    QByteArray cmd = freeArgs.first();
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("rtags");
    QCoreApplication::setApplicationName("rtags");

    if (Options::s_verbose)
        qDebug() << argsmap;

    if (cmd == "daemonize") {
        {
            Client client;
            if (client.connect(timeout)) {
                client.exec(QHash<QByteArray, QVariant>(), QList<QByteArray>() << "quit");
            }
        }

        // Ensure that the TemporaryFiles singleton gets initialized in a thread safe manner
        TemporaryFiles::instance()->init();

        qInstallMsgHandler(syslogMsgHandler);
        Daemon daemon;
        for (int i=0; i<10; ++i) {
            if (daemon.start()) {
                return app.exec();
            }
            usleep(timeout * 1000);
        }
        return -2;
    } else {
        Client client;
        if (!client.connect(timeout)) {
            if (cmd == "quit") {
                qWarning("Can't connect to rtags daemon");
                return 0;
            }
            if (getenv("RTAGS_AUTOSTART") || argsmap.contains("autostart")) {
                client.startDaemon(app.arguments());
                sleep(1); // ### hmmmm
                if (!client.connect(timeout)) {
                    qWarning("Can't connect to rtags daemon");
                    return 1;
                }
            }
        }
        if (client.isConnected()) {
            QHash<QByteArray, QVariant> replymap = client.exec(argsmap, freeArgs);
            const QByteArray reply = replymap.value("result").toByteArray();
            if (!reply.isEmpty())
                printf("%s\n", reply.constData());
            return 0;
        }
    }
    qWarning("Couldn't connect to daemon");
    return -1;
}

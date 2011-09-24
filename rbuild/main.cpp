#include <QCoreApplication>
#include <QString>
#include <QList>
#include <stdio.h>
#include "RBuild.h"
#include "Utils.h"
#include "PreCompile.h"
#include "ClangRunnable.h"
#include <syslog.h>

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
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("rtags");
    QCoreApplication::setApplicationName("rtags");

    PreCompile::setPath("/tmp");

    ClangRunnable::init();
    qInstallMsgHandler(syslogMsgHandler);
    RBuild rbuild;
    for (int i=1; i<argc; ++i) {
        rbuild.addMakefile(argv[i]);
    }

    const bool ret = app.exec();
    ClangRunnable::cleanup();
    return ret;
}

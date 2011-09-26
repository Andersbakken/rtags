#include <QCoreApplication>
#include <QString>
#include <QList>
#include <stdio.h>
#include "RBuild.h"
#include "Utils.h"
#include "PreCompile.h"
#include "ClangRunnable.h"
#include <syslog.h>
#include <getopt.h>
#include "Shared.h"

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
    fprintf(stderr, "%s%s: %s%s\n", colorStart,
            qPrintable(QDateTime::currentDateTime().toString()),
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
    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "update-db", 0, 0, 'u' },
        { "srcdir", 1, 0, 's' },
        { "db-file", 1, 0, 'f' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "huf:s:";
    int idx, longIndex;
    
    QCoreApplication app(argc, argv);
    QThread::currentThread()->setObjectName("main");
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("rtags");
    QCoreApplication::setApplicationName("rtags");

    PreCompile::setPath("/tmp");

    RBuild rbuild;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case 'h':
        case 's':
            break;
        case 'u':
            if (rbuild.databaseFile().isEmpty()) {
                char buf[PATH_MAX + 1];
                if (!findDB(buf, PATH_MAX)) {
                    printf("%s %d: if (!findDB(buf, PATH_MAX)) {\n", __FILE__, __LINE__);
                    return 1;
                }
                rbuild.setDatabaseFile(buf);
            }
            break;
        case 'f':
            break;
        }
    }

    ClangRunnable::init();
    qInstallMsgHandler(syslogMsgHandler);
        
    for (int i=1; i<argc; ++i) {
        if (!rbuild.addMakefile(argv[i])) {
            printf("%s %d: if (!rbuild.addMakefile(argv[i]))\n", __FILE__, __LINE__);
            return 1;
        }
    }

    const bool ret = app.exec();
    ClangRunnable::cleanup();
    return ret;
}

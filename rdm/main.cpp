#include <QCoreApplication>
#include "Rdm.h"
#include <getopt.h>

void usage(FILE *f)
{
    fprintf(f,
            "rdm ...options...\n"
            "  --help|-h               Display this page\n"
            "  --include-path|-I [arg] Add additional include path to clang\n"
            "  --include|-i [arg]      Add additional include directive to clang\n"
            "  --log-file|-l [arg]     Log to this file\n"
            "  --thread-count|-j [arg] Spawn this many threads for thread pool\n");
}

QtMsgHandler oldHandler = 0;
FILE *logFile = 0;
void msgHandler(QtMsgType t, const char* str)
{
    Q_ASSERT(logFile);
    fprintf(logFile, "%s: %s",
            QDateTime::currentDateTime().toString("dd/MM/yy hh:mm:ss").toLocal8Bit().constData(),
            str);
    fsync(fileno(logFile));
    oldHandler(t, str);
}


int main(int argc, char** argv)
{
    struct option opts[] = {
        { "help", no_argument, 0, 'h' },
        { "include-path", required_argument, 0, 'I' },
        { "include", required_argument, 0, 'i' },
        { "log-file", required_argument, 0, 'l' },
        { "thread-count", required_argument, 0, 'j' },
        { 0, 0, 0, 0 }
    };

    int jobs = QThread::idealThreadCount();

    QList<QByteArray> defaultArguments;
    forever {
        const int c = getopt_long(argc, argv, "hI:i:l:j:", opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 'h':
            usage(stdout);
            return 0;
        case 'j':
            jobs = atoi(optarg);
            if (jobs <= 0) {
                fprintf(stderr, "Can't parse argument to -j %s", optarg);
                return 1;
            }
            break;
        case 'I':
            defaultArguments.append("-I" + QByteArray(optarg));
            break;
        case 'i':
            defaultArguments.append("-include");
            defaultArguments.append(optarg);
            break;
        case 'l':
            logFile = fopen(optarg, "w");
            if (!logFile) {
                qWarning("Can't open %s for writing", optarg);
                return 1;
            }
            oldHandler = qInstallMsgHandler(msgHandler);
            break;
        case '?':
            usage(stderr);
            return 1;
        }
    }
    QThreadPool::globalInstance()->setMaxThreadCount(jobs);
    qDebug("Running with %d jobs\n", jobs);

    QCoreApplication app(argc, argv);
    
    Rdm rdm;
    if (!rdm.init())
        return 1;

    const int ret = app.exec();
    if (logFile)
        fclose(logFile);
    return ret;
}

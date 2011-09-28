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
#include <QtCore>

static CXChildVisitResult dumpTree(CXCursor cursor, CXCursor parent, CXClientData)
{
    for (CXCursor p=clang_getCursorSemanticParent(cursor); isValidCursor(p); p = clang_getCursorSemanticParent(p)) {
        printf("  ");
    }
    QString str;
    {
        QDebug dbg(&str);
        dbg << cursor << (clang_equalCursors(parent, clang_getCursorSemanticParent(cursor)))
            << (clang_equalCursors(parent, clang_getCursorLexicalParent(cursor)))
            << parent << clang_getCursorSemanticParent(cursor)
            << clang_getCursorSemanticParent(clang_getCursorSemanticParent(cursor));
    }
    str.remove("\"");
    printf("%s\n", qPrintable(str));
    return CXChildVisit_Recurse;
}

static inline void gatherHeaders(CXFile includedFile, CXSourceLocation*,
                                 unsigned includeLen, CXClientData userData)
{
    if (includeLen != 1)
        return;

    const Path path = Path::resolved(eatString(clang_getFileName(includedFile)));
    // qWarning() << filename << includeLen;

    if (!path.contains("/bits/")) {
        QSet<Path> *includes = reinterpret_cast<QSet<Path>*>(userData);
        includes->insert(path);
    }
}

static inline int findIncludes(const Path &path, CXIndex idx, QSet<Path> &includes, const char *const *args, int argCount)
{
    QFile f(path);
    if (!f.open(QIODevice::ReadOnly)) {
        qWarning("Can't open %s", path.constData());
        return -1;
    }

    QByteArray unsaved;
    unsaved.reserve(f.size() / 2);
    const QList<QByteArray> lines = f.readAll().split('\n');
    for (int i=0; i<lines.size(); ++i) {
        QByteArray line = lines.at(i).trimmed();
        if (line.startsWith('#')) {
            while (line.endsWith('\\') && i + 1 < lines.size()) {
                line.chop(1);
                line += lines.at(++i).trimmed();
            }
            unsaved.append(line);
            unsaved.append('\n');
        }
    }
    printf("%s\n", unsaved.constData());
    CXUnsavedFile unsavedFile = { path.constData(), unsaved.constData(), unsaved.size() };
    CXTranslationUnit unit = clang_parseTranslationUnit(idx, path.constData(), args, argCount,
                                                        &unsavedFile, 1, CXTranslationUnit_Incomplete);
    if (!unit) {
        qWarning("Can't parse file %s", path.constData());
        return -1;
    }
    const int count = includes.size();
    clang_getInclusions(unit, gatherHeaders, &includes);
    clang_disposeTranslationUnit(unit);
    return includes.size() - count;
}


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

static inline void usage(const char* argv0, FILE *f)
{
    fprintf(f,
            "%s [options]...\n"
            "  --help|-h                  Display this help\n"
            "  --update|-u [optional arg] Update database, using heuristics to find the file if arg is not supplied\n"
            "  --srcdir|-s [arg]          Build list of files from this directory\n",
            argv0);
}


int main(int argc, char** argv)
{
    class ClangRunnableScope
    {
    public:
        ClangRunnableScope() { ClangRunnable::init(); }
        ~ClangRunnableScope() { ClangRunnable::cleanup(); }
    } scope;
    
    if (QFile::exists("/tmp/rtags.log")) {
        int idx = 1;
        while (QFile::exists(QString("/tmp/rtags.log.%1").arg(idx)))
            ++idx;
        QFile::rename("/tmp/rtags.log", QString("/tmp/rtags.log.%1").arg(idx));
    }
    qInstallMsgHandler(syslogMsgHandler);

    struct option longOptions[] = {
        { "help", 0, 0, 'h' },
        { "update-db", optional_argument, 0, 'u' },
        { "srcdir", required_argument, 0, 's' },
        { 0, 0, 0, 0 },
    };
    const char *shortOptions = "hu::s:";
    int idx, longIndex;
    
    QCoreApplication app(argc, argv);
    QThread::currentThread()->setObjectName("main");
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("rtags");
    QCoreApplication::setApplicationName("rtags");

    PreCompile::setPath("/tmp");

    QList<QByteArray> compilerOptions;
    QVector<const char*> clangArgs;
    QByteArray empty("");
    for (int i=1; i<argc; ++i) {
        if (argv[i] && (!strncmp(argv[i], "-I", 2) || (!strncmp(argv[i], "-D", 2)))) {
            Path p = Path::resolved(argv[i] + 2);
            compilerOptions.append("-I" + p);
            clangArgs.append(compilerOptions.last().constData());
            argv[i] = empty.data();
        }
    }

    RBuild rbuild;
    bool update = false;
    const char *dbFile = 0;
    while ((idx = getopt_long(argc, argv, shortOptions, longOptions, &longIndex)) != -1) {
        switch (idx) {
        case 'h':
            usage(argv[0], stdout);
            return 0;
        case 's':
            printf("%s %d: case 's':\n", __FILE__, __LINE__);
            return 2;
        case 'u':
            update = true;
            if (optarg) {
                dbFile = *optarg == '=' ? optarg + 1 : optarg;
            } else if (optind < argc && *argv[optind] != '-') { // ### optind is off by one for some reason
                dbFile = argv[optind];
                argv[optind] = 0;
            }
            break;
        default:
            break;
        }
    }
    QElapsedTimer timer;
    timer.start();
    QSet<Path> includes;

    CXIndex index = clang_createIndex(1, 1);
    if (!getenv("NO_PCH")) {
        for (int i=1; i<argc; ++i) {
            if (argv[i] && *argv[i] != '-') {
                if (update) {
                    printf("%s %d: if (update) {\n", __FILE__, __LINE__);
                    return 1;
                } else if (strlen(argv[i])) {
                    findIncludes(Path::resolved(argv[i]), index, includes, clangArgs.constData(), clangArgs.size());
                    // if (!rbuild.addMakefile(argv[i])) {
                    //     qWarning("Couldn't add makefile \"%s\"", argv[i]);
                    //     return 1;
                    // }
                }
            }
        }
        qDebug() << includes;

        QByteArray pchHeader;
        // inc += "#include \"/home/abakken/dev/qt-47/include/QtCore/qhash.h\"\n";
        foreach(const Path &header, includes) {
            pchHeader += "#include \"" + header + "\"\n";
        }
        CXUnsavedFile unsavedFile = { "/tmp/magic.pch.h", pchHeader.constData(), pchHeader.size() };

        clangArgs.append("-x");
        clangArgs.append("c++");
    
        CXTranslationUnit unit = clang_parseTranslationUnit(index, "/tmp/magic.pch.h", clangArgs.constData(),
                                                            clangArgs.size(), &unsavedFile, 1,
                                                            CXTranslationUnit_Incomplete);
        // clang_visitChildren(clang_getTranslationUnitCursor(unit), dumpTree, 0);
        clang_saveTranslationUnit(unit, "/tmp/magic.pch", clang_defaultSaveOptions(unit));
        clang_disposeTranslationUnit(unit);
        qDebug() << "made a pch";
    }
    for (int i=1; i<argc; ++i) {
        if (argv[i] && *argv[i] != '-') {
            if (update) {
                printf("%s %d: if (update) {\n", __FILE__, __LINE__);
                return 1;
            } else if (strlen(argv[i])) {
                CXTranslationUnit unit = clang_parseTranslationUnit(index, Path::resolved(argv[i]),
                                                                    clangArgs.constData(), clangArgs.size(),
                                                                    0, 0, 0);
                printf("%p\n", unit);
                // clang_visitChildren(clang_getTranslationUnitCursor(unit), dumpTree, 0);
                clang_disposeTranslationUnit(unit);

                // if (!rbuild.addMakefile(argv[i])) {
                //     qWarning("Couldn't add makefile \"%s\"", argv[i]);
                //     return 1;
                // }
            }
        }
    }

    clang_disposeIndex(index);
    qDebug() << timer.elapsed();
    return 0;

    if (dbFile) {
        rbuild.setDatabaseFile(dbFile, update ? RBuild::Update : RBuild::Build);
    } else if (!rbuild.findDatabaseFile(update ? RBuild::Update : RBuild::Build)) {
        printf("%s %d: } else if (!rbuild.detectDatabaseFile(update ? RBuild::Update : RBuild::Build)) {\n", __FILE__, __LINE__);
        return 1;
    }

    if (rbuild.pendingWork()) {
        return app.exec();
    }

    printf("%s %d: return 0;\n", __FILE__, __LINE__);
    return 0;
}

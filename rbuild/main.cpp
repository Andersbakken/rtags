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
    if (clang_getCursorKind(cursor) == CXCursor_InclusionDirective) {
        qDebug() << cursor;
    }
    return CXChildVisit_Continue;
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


static QList<Path> findStdIncludePaths()
{
    QProcess process;
    process.start("cpp", QStringList() << "-v" << "-");
    process.closeWriteChannel();
    process.waitForFinished();
    QList<Path> paths;
    foreach(const QByteArray &line, process.readAllStandardError().split('\n')) {
        if (line.startsWith(" /")) {
            Path p = Path::resolved(line.mid(1));
            if (p.isDir())
                paths.append(p);
        }
    }

    return paths;
}

static inline void gatherHeaders(CXFile includedFile, CXSourceLocation*,
                                 unsigned includeLen, CXClientData userData)
{
    if (includeLen != 1)
        return;

    qDebug() << eatString(clang_getFileName(includedFile));
    const Path path = eatString(clang_getFileName(includedFile));// = Path::resolved(eatString(clang_getFileName(includedFile)));
    // qWarning() << filename << includeLen;
    if (!path.contains("/bits/")) {
        QSet<Path> *includes = reinterpret_cast<QSet<Path>*>(userData);
        includes->insert(path);
    }
}

static inline QList<Path> findIncludes(const Path &path, const QList<Path> &includePaths, const QList<Path> &stdSearchPaths)
{
    QElapsedTimer timer;
    timer.start();
    QList<Path> ret;
    QFile f(path);
    if (!f.open(QIODevice::ReadOnly)) {
        qWarning("Can't open %s", path.constData());
        return ret;
    }
    // qDebug() << __LINE__ << timer.restart() << path;

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
            if (line.startsWith("#include ")) {
                line.remove(0, 9);
            }

            unsaved.append(line);
            unsaved.append('\n');
        }
    }
    QProcess process;
    process.start("/usr/local/llvm/bin/clang", QStringList() << "-E" << "-");
    process.write(unsaved);
    process.closeWriteChannel();
    process.waitForFinished();
    const Path sourceFileDir = path.parentDir();
    foreach(QByteArray line, process.readAllStandardOutput().split('\n')) {
        if (!line.isEmpty()) {
            bool quote = true;
            switch (line.at(0)) {
            case '"':
                if (line.at(line.size() - 1) != '"') { // flag error?
                    qWarning() << "Weird include" << line;
                    continue;
                }
                break;
            case '<':
                if (line.at(line.size() - 1) != '>') { // flag error?
                    qWarning() << "Weird include" << line;
                    continue;
                }
                quote = false;
                break;
            default:
                continue;
            }
            line.remove(0, 1);
            line.chop(1);
            if (quote) {
                const Path resolved = Path::resolved(line, sourceFileDir);
                if (resolved.isHeader()) {
                    ret.append(resolved);
                    continue;
                }
            }
            const QList<Path> *lists[] = { &includePaths, &stdSearchPaths };
            bool found = false;
            for (int i=0; i<2 && !found; ++i) {
                foreach(const Path &dir, *lists[i]) {
                    const Path resolved = Path::resolved(line, dir);
                    if (resolved.isHeader()) {
                        ret.append(resolved);
                        found = true;
                        break;
                    }
                }
            }
            if (!found) {
                qDebug() << "Couldn't resolve" << line << includePaths << stdSearchPaths;
            }
        }
    }
    qDebug() << "returning" << ret << "for" << path;
    return ret;
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
    QList<Path> includePaths;
    QByteArray empty("");
    for (int i=1; i<argc; ++i) {
        if (argv[i]) {
            if (!strncmp(argv[i], "-I", 2)) {
                Path p = Path::resolved(argv[i] + 2);
                if (p.isDir())
                    includePaths.append(p);
                compilerOptions.append("-I" + p);
                clangArgs.append(compilerOptions.last().constData());
                argv[i] = empty.data();
            } else if (!strncmp(argv[i], "-D", 2)) {
                clangArgs.append(argv[i]);
                argv[i] = empty.data();
            }
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
    QList<Path> includes;

    const QList<Path> stdSearchPaths = findStdIncludePaths();
    CXIndex index = clang_createIndex(1, 1);
    for (int i=1; i<argc; ++i) {
        if (argv[i] && *argv[i] != '-') {
            if (update) {
                printf("%s %d: if (update) {\n", __FILE__, __LINE__);
                return 1;
            } else if (strlen(argv[i])) {
                const Path sourceFile = Path::resolved(argv[i]);
                if (sourceFile.isFile()) {
                    includes += findIncludes(sourceFile, includePaths, stdSearchPaths);

                    // if (!rbuild.addMakefile(argv[i])) {
                    //     qWarning("Couldn't add makefile \"%s\"", argv[i]);
                    //     return 1;
                    // }
                }
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

    qDebug() << clangArgs;
    CXTranslationUnit unit = clang_parseTranslationUnit(index, "/tmp/magic.pch.h", clangArgs.constData(),
                                                        clangArgs.size(), &unsavedFile, 1,
                                                        CXTranslationUnit_Incomplete);
    ClangRunnable::processTranslationUnit("/tmp/magic.pch.h", unit);
    // clang_visitChildren(clang_getTranslationUnitCursor(unit), dumpTree, 0);
    clang_saveTranslationUnit(unit, "/tmp/magic.pch", clang_defaultSaveOptions(unit));
    clang_disposeTranslationUnit(unit);
    qDebug() << "made a pch" << includes << timer.elapsed();
    clangArgs.append("-include-pch");
    clangArgs.append("/tmp/magic.pch");
    for (int i=1; i<argc; ++i) {
        if (argv[i] && *argv[i] != '-') {
            if (update) {
                printf("%s %d: if (update) {\n", __FILE__, __LINE__);
                return 1;
            } else if (strlen(argv[i])) {
                Path p = Path::resolved(argv[i]);
                CXTranslationUnit unit = clang_parseTranslationUnit(index, p,
                                                                    clangArgs.constData(), clangArgs.size(),
                                                                    &unsavedFile, 1, 0);
                QElapsedTimer tm;
                tm.start();
                ClangRunnable::processTranslationUnit(p, unit);

                qDebug() << tm.elapsed() << p;
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

#include "Daemon.h"
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "PreCompile.h"
#include "Node.h"
#include "FileManager.h"
#include <magic.h>
#include <fcntl.h>

// const unsigned defaultFlags = (CXTranslationUnit_PrecompiledPreamble
//                                |CXTranslationUnit_CXXPrecompiledPreamble
//                                |CXTranslationUnit_CXXChainedPCH);

template <typename T>
static inline QByteArray joined(const T &container, const char joinCharacter = '\n')
{
    QByteArray joined;
    joined.reserve(container.size() * 100);
    foreach(const QByteArray &f, container) {
        joined += f + joinCharacter;
    }
    if (!joined.isEmpty())
        joined.chop(1);
    return joined;
}

const unsigned defaultFlags = 0;

static QHash<QByteArray, QVariant> createResultMap(const QByteArray& result)
{
    QHash<QByteArray, QVariant> ret;
    ret.insert("result", result);
    return ret;
}

Daemon::Daemon(QObject *parent)
    : QObject(parent), mParseThread(&mFileManager, &mVisitThread), mFileManager(&mParseThread)
{
    qRegisterMetaType<Path>("Path");
    qRegisterMetaType<QSet<Path> >("QSet<Path>");
    qRegisterMetaType<CXTranslationUnit>("CXTranslationUnit");
    connect(&mParseThread, SIGNAL(fileParsed(Path, void*)), &mVisitThread, SLOT(onFileParsed(Path, void*)));
    connect(&mParseThread, SIGNAL(parseError(Path)), &mVisitThread, SLOT(onParseError(Path)));
    connect(&mVisitThread, SIGNAL(done()), this, SLOT(onMaybeDone()));
    connect(&mFileManager, SIGNAL(done()), this, SLOT(onMaybeDone()));
    mParseThread.start();
    mVisitThread.start();
}

Daemon::~Daemon()
{
    mParseThread.abort();
    mVisitThread.quit();
    Q_ASSERT(mFileManager.isDone());
    QThread *threads[] = { &mParseThread, &mVisitThread, 0 };
    for (int i=0; threads[i]; ++i)
        threads[i]->wait();
}

static QHash<QByteArray, QVariant> syntax()
{
    return createResultMap("Syntax: rtags --command=command [--argument1, --argument2=foo, ...]\n"
                           "commands: syntax|quit|add|remove|lookupline|makefile|daemonize|files|lookup\n");
}

void Daemon::quit()
{
    mParseThread.abort();
    mVisitThread.abort();
    mParseThread.wait();
    mVisitThread.wait();
    QTimer::singleShot(100, QCoreApplication::instance(), SLOT(quit()));
    // hack to make the quit command properly respond before the server goes down
}

void Daemon::addMakefile(Path makefile)
{
    if (!makefile.isResolved())
        makefile.resolve();
    if (makefile.isDir())
        makefile = makefile + "/Makefile";
    if (makefile.isFile())
        mFileManager.addMakefile(makefile);
}


QDebug operator<<(QDebug dbg, CXCursor cursor)
{
    QString text = "";
    if (clang_isInvalid(clang_getCursorKind(cursor))) {
        text += "";
        dbg << text;
        return dbg;
    }

    QByteArray name = eatString(clang_getCursorDisplayName(cursor));
    if (name.isEmpty())
        name = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty()) {
        text += name + ", ";
    }
    if (clang_isCursorDefinition(cursor))
        text += "(def), ";
    text += kindToString(clang_getCursorKind(cursor));
    CXSourceLocation location = clang_getCursorLocation(cursor);
    unsigned int line, column, offset;
    CXFile file;
    clang_getInstantiationLocation(location, &file, &line, &column, &offset);
    Path path = eatString(clang_getFileName(file));
    if (path.resolve()) {
        text += QString(", %1:%2:%3").arg(QString::fromLocal8Bit(path)).arg(line).arg(column);
    }
    if (clang_isCursorDefinition(cursor))
        text += ", def";
    dbg << text;
    return dbg;
}

void Daemon::onMaybeDone()
{
    extern int addedFiles;
    if (!addedFiles && mFileManager.isDone())
        quit();
}

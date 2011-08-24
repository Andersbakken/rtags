#include "Daemon.h"
#ifndef EBUS_ENABLED
#include "DaemonAdaptor.h"
#endif
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "PreCompile.h"
#include "Node.h"

// const unsigned defaultFlags = (CXTranslationUnit_PrecompiledPreamble
//                                |CXTranslationUnit_CXXPrecompiledPreamble
//                                |CXTranslationUnit_CXXChainedPCH);

struct VisitData {
    enum { BufferLength = 1024 };
    char buffer[BufferLength];
    QByteArray output;
};

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

// static inline QDebug operator<<(QDebug dbg, CXCursor cursor)
// {
//     QString text = "";
//     if (clang_isInvalid(clang_getCursorKind(cursor))) {
//         text += "";
//         dbg << text;
//         return dbg;
//     }

//     QByteArray name = eatString(clang_getCursorDisplayName(cursor));
//     if (name.isEmpty())
//         name = eatString(clang_getCursorSpelling(cursor));
//     if (!name.isEmpty()) {
//         text += name + ", ";
//     }
//     text += kindToString(clang_getCursorKind(cursor));
//     CXSourceLocation location = clang_getCursorLocation(cursor);
//     unsigned int line, column, offset;
//     CXFile file;
//     clang_getInstantiationLocation(location, &file, &line, &column, &offset);
//     Path path = eatString(clang_getFileName(file));
//     if (path.resolve()) {
//         text += QString(", %1:%2:%3").arg(QString::fromLocal8Bit(path).split('/', QString::SkipEmptyParts).last()).arg(line).arg(column);
//     }
//     if (clang_isCursorDefinition(cursor))
//         text += ", def";
//     dbg << text;
//     return dbg;
// }


Daemon::Daemon(QObject *parent)
    : QObject(parent)
{
    qRegisterMetaType<Path>("Path");
    qRegisterMetaType<CXTranslationUnit>("CXTranslationUnit");
    mParseThread.start();
    mVisitThread.start();
    connect(&mParseThread, SIGNAL(fileParsed(Path, void*)), &mVisitThread, SLOT(onFileParsed(Path, void*)));
    connect(&mParseThread, SIGNAL(invalidated(Path)), &mVisitThread, SLOT(invalidate(Path)));
}

Daemon::~Daemon()
{
}

bool Daemon::start()
{
    FUNC;
#ifndef EBUS_ENABLED
    DaemonAdaptor* adaptor = new DaemonAdaptor(this);

    QDBusConnection dbus = QDBusConnection::sessionBus();
    if (!dbus.registerObject(QLatin1String("/"), this)) {
        delete adaptor;
        return false;
    }
    if (!dbus.registerService(QLatin1String("rtags.Daemon"))) {
        delete adaptor;
        return false;
    }

    return true;
#else
    if (!mEbus.start())
        return false;
    connect(&mEbus, SIGNAL(ebusConnected(EBus*)), this, SLOT(ebusConnected(EBus*)));
    return true;
#endif
}

#ifdef EBUS_ENABLED
void Daemon::ebusConnected(EBus *ebus)
{
    connect(ebus, SIGNAL(ready()), this, SLOT(ebusDataReady()));
}

void Daemon::ebusDataReady()
{
    EBus* ebus = qobject_cast<EBus*>(sender());
    if (!ebus)
        return;

    static int bytearrayhash = QMetaType::type("ByteArrayHash");
    static int bytearraylist = QMetaType::type("QList<QByteArray>");
    Q_UNUSED(bytearrayhash);
    Q_ASSERT(ebus->peek() == bytearrayhash);
    QHash<QByteArray, QVariant> args = ebus->pop().value<QHash<QByteArray, QVariant> >();
    Q_ASSERT(ebus->peek() == bytearraylist);
    Q_UNUSED(bytearraylist);
    QList<QByteArray> list = ebus->pop().value<QList<QByteArray> >();

    QHash<QByteArray, QVariant> ret = runCommand(args, list);

    QVariant ebusarg = qVariantFromValue(ret);
    ebus->push(ebusarg);
    ebus->send();
}
#endif

static QHash<QByteArray, QVariant> syntax()
{
    FUNC;
    return createResultMap("Syntax: rtags --command=command [--argument1, --argument2=foo, ...]\n"
                           "commands: syntax|quit|add|remove|lookupline|makefile|daemonize|files|lookup\n");
}

QHash<QByteArray, QVariant> Daemon::runCommand(const QHash<QByteArray, QVariant> &dashArgs,
                                               const QList<QByteArray>& freeArgs)
{
    FUNC2(dashArgs, freeArgs);

    QString cmd = dashArgs.value("command").toString();
    if (cmd.isEmpty())
        return createResultMap("No command or path specified");

    if (cmd == "syntax") {
        return syntax();
    } else if (cmd == "quit") {
        mParseThread.abort();
        mVisitThread.quit();
        mParseThread.wait();
        mVisitThread.wait();
        QTimer::singleShot(100, QCoreApplication::instance(), SLOT(quit()));
        // hack to make the quit command properly respond before the server goes down
        return createResultMap("quitting");
    } else if (cmd == "add") {
        return addSourceFile(dashArgs);
    } else if (cmd == "remove") {
        return removeSourceFile(dashArgs, freeArgs);
    } else if (cmd == "printtree") {
        mVisitThread.printTree();
        return createResultMap("Done");;
    } else if (cmd == "lookupline") {
        return lookupLine(dashArgs);
    } else if (cmd == "makefile") {
        return addMakefile(dashArgs, freeArgs);
    } else if (cmd == "files") {
        return fileList(dashArgs);
    } else if (cmd == "lookup") {
        return lookup(dashArgs, freeArgs);
    } else if (cmd == "load") {
        return load(dashArgs, freeArgs);
    } else if (cmd == "complete") {
        return complete(dashArgs, freeArgs);
    }
    return createResultMap("Unknown command");
}

static bool contains(const Path &key, const QRegExp &rx)
{
    return QString::fromLocal8Bit(key).contains(rx);
}

static bool contains(const Path &key, const QByteArray &b)
{
    return key.contains(b);
}

template <typename T>
static QSet<Path> matches(const QSet<Path> &files, const T &t)
{
    QSet<Path> matches;
    foreach(const Path &path, files) {
        if (contains(path, t))
            matches += path;
    }
    return matches;
}

QHash<QByteArray, QVariant> Daemon::fileList(const QHash<QByteArray, QVariant> &args)
{
    FUNC1(args);
    bool regexp = true;
    QByteArray pattern;
    if (pattern.isEmpty())
        pattern = args.value("regexp").toByteArray();
    if (pattern.isEmpty()) {
        pattern = args.value("match").toByteArray();
        regexp = false;
    }
    QSet<Path> files = mVisitThread.files();
    QSet<Path> out;
    if (pattern.isEmpty()) {
        out = files;
    } else if (regexp) {
        QRegExp rx(pattern);
        out = matches(files, rx);
    } else {
        out = matches(files, pattern);
    }
    return createResultMap(joined(out));
}

QHash<QByteArray, QVariant> Daemon::addSourceFile(const QHash<QByteArray, QVariant> &args)
{
    // ### should use free args
    FUNC1(args);

    Path file = args.value("file").toByteArray();
    if (file.isEmpty())
        return createResultMap("No file to add (use --file=<file>)");
    if (!file.resolve())
        return createResultMap(file + " Doesn't exist");
    mParseThread.addFile(file, GccArguments());
    return createResultMap("File added");
}

QHash<QByteArray, QVariant> Daemon::addMakefile(const QHash<QByteArray, QVariant>& dashArgs,
                                                const QList<QByteArray>& freeArgs)
{
    FUNC2(dashArgs, freeArgs);

    Q_UNUSED(dashArgs);

    if (freeArgs.isEmpty())
        return createResultMap("No Makefile passed");

    Path makefile = freeArgs.first();
    if (!makefile.isResolved())
        makefile.resolve();
    if (!makefile.isFile()) {
        return createResultMap("Makefile does not exist: " + makefile);
    }
    QRegExp accept(dashArgs.value("accept").toString());
    QRegExp reject(dashArgs.value("reject").toString());
    
    mParseThread.addMakefile(makefile, accept, reject);
    return createResultMap("Added makefile");
}

QHash<QByteArray, QVariant> Daemon::removeSourceFile(const QHash<QByteArray, QVariant> &args,
                                                     const QList<QByteArray> &freeArgs)
{
    FUNC1(args);
    const bool regexp = (args.contains("regexp") || args.contains("r"));
    if (freeArgs.size() != 1 || freeArgs.first().isEmpty())
        return createResultMap("Invalid arguments. I need exactly one free arg");
    QRegExp rx;
    QByteArray match;
    if (regexp) {
        rx.setPattern(freeArgs.first());
        if (!rx.isValid() || rx.isEmpty())
            return createResultMap("Invalid arguments. Bad regexp");
    } else {
        match = freeArgs.first();
    }

    QList<QByteArray> removed;
    QHash<Path, CXTranslationUnit>::iterator it = mTranslationUnits.begin();
    while (it != mTranslationUnits.end()) {
        if ((regexp && QString::fromLocal8Bit(it.key()).contains(rx))
            || (!regexp && it.key().contains(match))) {
            clang_disposeTranslationUnit(it.value());
            it = mTranslationUnits.erase(it);
            removed.append(it.key());
        } else {
            ++it;
        }
    }

    if (removed.isEmpty())
        return createResultMap("No matches for " + freeArgs.first());

    return createResultMap("Removed " + joined(removed));
}

static inline void visitCallbackLocation(const Node *node, const QByteArray &qualifiedSymbolName, void *userData)
{
    switch (node->type) {
    case Node::MethodDeclaration:
        node = node->methodDefinition();
        break;
    case Node::MethodDefinition:
        node = node->methodDeclaration();
        break;
    case Node::MethodReference:
        node = node->methodDefinition();
        break;
    default:
        break;
    }
    Q_ASSERT(node);
    VisitData *data = reinterpret_cast<VisitData*>(userData);
    snprintf(data->buffer, VisitData::BufferLength, "%s %s \"%s:%d:%d\"\n",
             Node::typeToName(node->type, true), qualifiedSymbolName.constData(),
             node->location.path.constData(), node->location.line, node->location.column);
    data->output.append(data->buffer);
}


QHash<QByteArray, QVariant> Daemon::lookupLine(const QHash<QByteArray, QVariant> &args)
{
    FUNC1(args);
    if (!args.contains("file")
        || !args.contains("line")
        || !args.contains("column"))
        return createResultMap("Invalid argument count");

    Path file = args.value("file").toByteArray();
    if (file.isResolved())
        file.resolve();
    int line = args.value("line").toInt();
    int column = args.value("column").toInt();

    if (!file.isFile() || line == 0 || column == 0)
        return createResultMap("Invalid argument type");

    if (!mTranslationUnits.value(file)) {
        VisitData visitData;
        mVisitThread.lookup((QList<QByteArray>() << file << QByteArray::number(line) << QByteArray::number(column)),
                            VisitThread::MatchLocation, (Node::All & ~Node::Root),
                            ::visitCallbackLocation, &visitData);
        return createResultMap(visitData.output);
    }

    CXTranslationUnit unit = mTranslationUnits.value(file);
    CXFile f = clang_getFile(unit, file.constData());

    CXSourceLocation location = clang_getLocation(unit, f, line, column);
    CXCursor cursor = clang_getCursor(unit, location);
    if (!isValidCursor(cursor))
        return createResultMap("Unable to get cursor for location");

    CXCursorKind kind = clang_getCursorKind(cursor);
    CXCursor referenced;
    if (kind == CXCursor_CXXMethod) { // might need to add more here
        referenced = clang_getCanonicalCursor(cursor);
    } else {
        referenced = clang_getCursorReferenced(cursor);
    }
    if (!isValidCursor(referenced))
        return createResultMap("No referenced cursor");

    location = clang_getCursorLocation(referenced);
    unsigned int rline, rcolumn, roffset;
    CXFile rfile;
    clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
    CXString rfilename = clang_getFileName(rfile);

    char ret[64];
    snprintf(ret, 63, "Symbol (decl) at %s, line %u column %u",
             clang_getCString(rfilename), rline, rcolumn);
    clang_disposeString(rfilename);
    return createResultMap(ret);
}


static Node::Type stringToType(const QByteArray &in)
{
    for (int i=Node::MethodDeclaration; i<=Node::EnumValue; i <<= 1) {
        const Node::Type type = static_cast<Node::Type>(i);
        const char *name = Node::typeToName(type, true);
        Q_ASSERT(name);
        if (!strcasecmp(name, in.constData())) {
            return static_cast<Node::Type>(i);
        }
    }
    return Node::None;
}


static inline void visitCallback(const Node *node, const QByteArray &qualifiedSymbolName, void *userData)
{
    VisitData *data = reinterpret_cast<VisitData*>(userData);
    snprintf(data->buffer, VisitData::BufferLength, "%s %s \"%s:%d:%d\"\n",
             Node::typeToName(node->type, true), qualifiedSymbolName.constData(),
             node->location.path.constData(), node->location.line, node->location.column);
    data->output.append(data->buffer);
}

QHash<QByteArray, QVariant> Daemon::lookup(const QHash<QByteArray, QVariant> &args, const QList<QByteArray> &freeArgs)
{
    uint nodeTypes = 0;
    foreach(const QByteArray &type, args.value("types").toByteArray().split(',')) {
        if (type.isEmpty())
            continue;
        const Node::Type t = stringToType(type);
        if (t) {
            nodeTypes |= t;
        } else {
            return createResultMap("Can't parse type " + type);
        }
    }
    if (!nodeTypes)
        nodeTypes = (Node::All & ~Node::Root);

    uint flags = 0;
    if (args.contains("regexp"))
        flags |= VisitThread::MatchRegExp;
    if (args.contains("filename"))
        flags |= VisitThread::MatchFileNames;
    if (args.contains("symbolname") || !(flags & (VisitThread::MatchFileNames)))
        flags |= VisitThread::MatchSymbolName;

    VisitData visitData;
    mVisitThread.lookup(freeArgs, flags, nodeTypes, ::visitCallback, &visitData);
    return createResultMap(visitData.output);
}

bool Daemon::writeAST(const QHash<Path, CXTranslationUnit>::const_iterator it)
{
    if (it == mTranslationUnits.end())
        return false;
    // FUNC;

    Path full = QCoreApplication::applicationDirPath().toLocal8Bit() + "/ast" + it.key();
    Path parentDir = full.parentDir();
    QDir dir(parentDir);
    if (!dir.exists())
        dir.mkpath(QString::fromLocal8Bit(parentDir));

    const int ret = clang_saveTranslationUnit(it.value(), full.constData(),
                                              clang_defaultSaveOptions(it.value()));
    return (ret == 0);
}

void Daemon::onFileParsed(const Path &path, void *translationUnit)
{
    if (mTranslationUnits.contains(path)) {
        clang_disposeTranslationUnit(mTranslationUnits.value(path));
        qDebug() << "reparsed for completion" << path;
    } else {
        qDebug() << "parsed" << path << "for completion";
    }
    mTranslationUnits[path] = reinterpret_cast<CXTranslationUnit>(translationUnit);
}
QHash<QByteArray, QVariant> Daemon::load(const QHash<QByteArray, QVariant>&,
                                         const QList<QByteArray> &freeArgs)
{
    Path filename = freeArgs.value(0);
    if (!filename.isResolved())
        filename.resolve();
    if (!filename.isFile())
        return createResultMap("No filename specified");
    const QHash<Path, CXTranslationUnit>::iterator it = mTranslationUnits.find(filename);
    if (it != mTranslationUnits.end()) {
        if (!it.value())
            return createResultMap("File already loading " + filename);
        clang_disposeTranslationUnit(it.value());
        mTranslationUnits.erase(it);
    }
    mTranslationUnits[filename] = 0;
    mParseThread.loadTranslationUnit(filename, this, "onFileParsed");
    return createResultMap("Loading");
}
QHash<QByteArray, QVariant> Daemon::complete(const QHash<QByteArray, QVariant>& args,
                                             const QList<QByteArray> &freeArgs)
{
    QElapsedTimer timer;
    timer.start();
    const Path file = freeArgs.value(0);
    if (!file.isFile())
        return createResultMap("Invalid file " + freeArgs.value(0));
    CXTranslationUnit unit = mTranslationUnits.value(file);
    if (!unit)
        return createResultMap(file + " is not loaded");

    const unsigned line = args.value("line", UINT_MAX).toUInt();
    const unsigned column = args.value("column", UINT_MAX).toUInt();
    if (line == UINT_MAX || column == UINT_MAX)
        return createResultMap("Invalid args. Need both column and line");

    static const unsigned options = CXCompletionContext_ObjCInterface; //CXCompletionContext_DotMemberAccess;
    CXCodeCompleteResults *res = clang_codeCompleteAt(unit, file.constData(),
                                                      line, column, 0, 0,
                                                      options);
    if (!res)
        return createResultMap("Can't complete here for this. You'd probably want a better error message");
    QByteArray results;
    for (unsigned i=0; i<res->NumResults; ++i) {
        CXCompletionResult r = res->Results[i];
        // switch (r.CursorKind) {
        // case CXCursor_NotImplemented:
        // case CXCursor_MacroDefinition:
        //     continue;
        // default:
        //     break;
        // }
        const unsigned count = clang_getNumCompletionChunks(r.CompletionString);
        QByteArray result;
        for (unsigned j=0; j<count; ++j) {
            if (j)
                result += ' ';
            result += eatString(clang_getCompletionChunkText(r.CompletionString, j)); // copies unnecessarily
            result += '(';
            result += clang_getCompletionChunkKind(r.CompletionString, j);
            result += ')';

        }
        results += " (";
        results += kindToString(r.CursorKind);
        results += ')';
        results += " ";
        results += QByteArray::number(clang_getCompletionPriority(r.CompletionString));
        if (!results.isEmpty())
            results += '\n';
        results += result;
    }

    clang_disposeCodeCompleteResults(res);
    // ### need to allow for unsaved files
    qDebug() << "completion took" << timer.elapsed();
    return createResultMap(results);
}

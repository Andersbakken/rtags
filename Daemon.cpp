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
const unsigned defaultFlags = 0;

// ### might be worth optimizing

const QByteArray dirName(const QByteArray &filePath)
{
    Q_ASSERT(!filePath.endsWith('/'));
    return filePath.left(filePath.lastIndexOf('/'));
}

static QHash<QByteArray, QVariant> createResultMap(const QByteArray& result)
{
    QHash<QByteArray, QVariant> ret;
    ret.insert("result", result);
    return ret;
}

static inline QDebug operator<<(QDebug dbg, CXCursor cursor)
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
    text += kindToString(clang_getCursorKind(cursor));
    CXSourceLocation location = clang_getCursorLocation(cursor);
    unsigned int line, column, offset;
    CXFile file;
    clang_getInstantiationLocation(location, &file, &line, &column, &offset);
    Path path = eatString(clang_getFileName(file));
    if (path.resolve()) {
        text += QString(", %1:%2:%3").arg(QString::fromLocal8Bit(path).split('/', QString::SkipEmptyParts).last()).arg(line).arg(column);
    }
    if (clang_isCursorDefinition(cursor))
        text += ", def";
    dbg << text;
    return dbg;
}


Daemon::Daemon(QObject *parent)
    : QObject(parent)
{
    qRegisterMetaType<Path>("Path");
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
    if (!m_ebus.start())
        return false;
    connect(&m_ebus, SIGNAL(ebusConnected(EBus*)), this, SLOT(ebusConnected(EBus*)));
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
        return removeSourceFile(dashArgs);
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
        return lookup(dashArgs);
    } else if (cmd == "loadast") {
        return loadAST(dashArgs);
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
    QByteArray joined;
    joined.reserve(out.size() * 100);
    foreach(const QByteArray &f, out) {
        joined += f + '\n';
    }
    if (!joined.isEmpty())
        joined.chop(1);
    return createResultMap(joined);
}

QHash<QByteArray, QVariant> Daemon::addSourceFile(const QHash<QByteArray, QVariant> &args)
{
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

    Path makefile;
    if (freeArgs.isEmpty()) {
        makefile = Path::resolved("Makefile");
    } else {
        makefile = freeArgs.first();
        if (!makefile.isResolved())
            makefile.resolve();
    }
    if (!makefile.isFile()) {
        return createResultMap("Makefile does not exist: " + makefile);
    }
    QRegExp accept(dashArgs.value("accept").toString());
    QRegExp reject(dashArgs.value("reject").toString());
    
    mParseThread.addMakefile(makefile, accept, reject);
    return createResultMap("Added makefile");
}

QHash<QByteArray, QVariant> Daemon::removeSourceFile(const QHash<QByteArray, QVariant> &args)
{
    // FUNC1(args);
    // bool regexp = true;
    // QByteArray pattern = args.value("regexp").toByteArray();
    // if (pattern.isEmpty()) {
    //     pattern = args.value("file").toByteArray();
    //     regexp = false;
    // }

    // // ### need to use regexp and match partial and all that good stuff. Maybe
    // // ### make it use the same code path as fileList
    // if (pattern.isEmpty())
    //     return createResultMap("No file to remove (use --file=<filename> or --regexp=.*file.*");
    // QHash<QByteArray, CXTranslationUnit>::iterator it = m_files.find(pattern);
    // if (it == m_files.end())
    //     return createResultMap("No matches for " + pattern);
    // clang_disposeTranslationUnit(it.value());
    // m_fileSystemWatcher.removePath(it.key());
    // m_files.erase(it);

    // return createResultMap("Removed");
}

QHash<QByteArray, QVariant> Daemon::lookupLine(const QHash<QByteArray, QVariant> &args)
{
    // FUNC1(args);
    // if (!args.contains("line")
    //     || !args.contains("line")
    //     || !args.contains("column"))
    //     return createResultMap("Invalid argument count");

    // QByteArray filename = args.value("file").toByteArray();
    // int line = args.value("line").toInt();
    // int column = args.value("column").toInt();

    // if (filename.isEmpty() || line == 0 || column == 0)
    //     return createResultMap("Invalid argument type");

    // if (!m_files.contains(filename))
    //     return createResultMap("Translation unit not found");

    // CXTranslationUnit unit = m_files.value(filename);
    // CXFile file = clang_getFile(unit, filename.constData());

    // CXSourceLocation location = clang_getLocation(unit, file, line, column);
    // CXCursor cursor = clang_getCursor(unit, location);
    // if (!isValidCursor(cursor))
    //     return createResultMap("Unable to get cursor for location");

    // CXCursorKind kind = clang_getCursorKind(cursor);
    // CXCursor referenced;
    // if (kind == CXCursor_CXXMethod) // might need to add more here
    //     referenced = clang_getCanonicalCursor(cursor);
    // else
    //     referenced = clang_getCursorReferenced(cursor);
    // if (!isValidCursor(referenced))
    //     return createResultMap("No referenced cursor");

    // location = clang_getCursorLocation(referenced);
    // unsigned int rline, rcolumn, roffset;
    // CXFile rfile;
    // clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
    // CXString rfilename = clang_getFileName(rfile);

    // char ret[64];
    // snprintf(ret, 63, "Symbol (decl) at %s, line %u column %u",
    //          clang_getCString(rfilename), rline, rcolumn);
    // clang_disposeString(rfilename);
    // return createResultMap(ret);
}

static inline QByteArray path(CXCursor cursor)
{
    QByteArray path;
    bool done = false;
    CXString tmp;
    do {
        cursor = clang_getCursorLexicalParent(cursor);
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_ClassDecl:
        case CXCursor_Namespace:
            tmp = clang_getCursorDisplayName(cursor);
            path.prepend("::");
            path.prepend(clang_getCString(tmp));
            clang_disposeString(tmp);
            break;
        default:
            done = true;
            break;
        }
    } while (!done);
    return path;
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

struct VisitData {
    enum { BufferLength = 1024 };
    char buffer[BufferLength];
    QByteArray output;
};

void visitCallback(const Node *node, const QByteArray &qualifiedSymbolName, void *userData)
{
    VisitData *data = reinterpret_cast<VisitData*>(userData);
    snprintf(data->buffer, VisitData::BufferLength, "%s %s \"%s:%d:%d\"\n",
             Node::typeToName(node->type, true), qualifiedSymbolName.constData(),
             node->location.path.constData(), node->location.line, node->location.column);
}

QHash<QByteArray, QVariant> Daemon::lookup(const QHash<QByteArray, QVariant> &args)
{
    const QByteArray symbol = args.value("symbol").toByteArray();
    if (symbol.isEmpty()) 
        return createResultMap("No symbol in lookup request");

    uint nodeTypes = 0;
    foreach(const QByteArray &type, args.value("types").toByteArray().split(',')) {
        if (type.isEmpty())
            continue;
        const Node::Type t = stringToType(type);
        if (t) {
            nodeTypes |= t;
        } else {
            qWarning("Can't parse type %s", type.constData());
        }
    }
    if (!nodeTypes)
        nodeTypes = (Node::All & ~Node::Root);

    uint flags = 0;
    if (args.contains("regexp"))
        flags |= VisitThread::RegExp;

    VisitData visitData;
    mVisitThread.lookup(symbol, flags, nodeTypes, ::visitCallback, &visitData);
    return createResultMap(visitData.output);
}

QHash<QByteArray, QVariant> Daemon::loadAST(const QHash<QByteArray, QVariant> &args)
{
    // QByteArray filename = args.value("file").toByteArray();
    // if (filename.isEmpty())
    //     return createResultMap("No filename specified (use --file=<filename>)");
    // if (m_files.contains(filename))
    //     return createResultMap("File already loaded");
    // QByteArray file = QCoreApplication::applicationDirPath().toLocal8Bit() + "/ast" + filename;
    // if (!resolvePath(file))
    //     return createResultMap("AST file does not exist");
    // CXTranslationUnit unit = clang_createTranslationUnit(m_index, file.constData());
    // // m_files[filename] = unit;
    // clang_disposeTranslationUnit(unit);
    // return createResultMap("AST file loaded");
}

bool Daemon::writeAST(const Path &path, CXTranslationUnit unit)
{
    // FUNC;

    // QFileInfo finfo(QCoreApplication::applicationDirPath() + "/ast" + filename);
    // QDir dir(finfo.absolutePath());
    // if (!dir.exists())
    //     dir.mkpath(finfo.absolutePath());

    // QByteArray outname = finfo.absoluteFilePath().toLocal8Bit();
    // const int ret = clang_saveTranslationUnit(unit, outname.constData(),
    //                                           clang_defaultSaveOptions(unit));
    // return (ret == 0);
}

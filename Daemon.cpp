#include "Daemon.h"
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "PreCompile.h"
#include "Node.h"
#include "FileManager.h"
#include "TemporaryFiles.h"

// const unsigned defaultFlags = (CXTranslationUnit_PrecompiledPreamble
//                                |CXTranslationUnit_CXXPrecompiledPreamble
//                                |CXTranslationUnit_CXXChainedPCH);

struct MatchBase : public Match
{
    enum Flags {
        MatchSymbolName = 0x01,
        MatchFileNames = 0x02,
        MatchRegExp = 0x04,
        SymbolOnly = 0x08,
        OneMatch = 0x10
    };

    MatchBase(uint nodeTypes, uint f)
        : Match(nodeTypes), flags(f)
    {}
    virtual MatchResult match(const QByteArray &path, const Node *node)
    {
        if (accept(path, node)) {
            int len;
            if (flags & SymbolOnly) {
                len = snprintf(buffer, BufferLength, "%s\n", node->symbolName.constData());
            } else {
                len = snprintf(buffer, BufferLength, "%s %s%s \"%s:%d:%d\"\n",
                               Node::typeToName(node->type, true), path.constData(), node->symbolName.constData(),
                               node->location.path.constData(), node->location.line, node->location.column);
            }
            output.append(buffer); // ### use len and QByteArray::fromRawData
            return (flags & OneMatch ? Finish : Recurse);
        }
        return Skip;
    }

    virtual bool accept(const QByteArray &path, const Node *node) = 0;

    const uint flags;
    enum { BufferLength = 1024 };
    char buffer[BufferLength];
    QByteArray output;
};

struct FollowSymbolMatch : public Match
{
    FollowSymbolMatch(const Location &loc)
        : Match(Node::All), location(loc), found(0)
    {}
    virtual MatchResult match(const QByteArray &, const Node *node)
    {
        // qDebug() << node->location << location << (node->location == location);
        if (node->location == location) {
            switch (node->type) {
            case Node::All:
            case Node::None:
            case Node::Root:
                break;
            case Node::MethodDeclaration:
                found = node->methodDefinition();
                break;
            case Node::Reference:
                found = node->parent;
                break;
            case Node::MethodDefinition:
                found = node->methodDeclaration();
                break;
            case Node::Class:
            case Node::Struct:
            case Node::Namespace:
            case Node::Variable:
            case Node::Enum:
                break;
            case Node::EnumValue:
                node = node->parent; // parent is Enum
                break;
            }
            return Finish;
        }

        return Recurse;
    }
    const Location &location;
    Node *found;
};

struct GenericMatch : public MatchBase
{
    GenericMatch(uint nodeTypes, uint flags, const QRegExp &r, const QByteArray &m)
        : MatchBase(nodeTypes, flags), regexp(r), match(m)
    {
    }
    virtual bool accept(const QByteArray &path, const Node *node)
    {
        if (flags & MatchFileNames) {
            if (!match.isEmpty() && node->location.path.contains(match))
                return true;
            if (flags & MatchRegExp && QString::fromLocal8Bit(node->location.path).contains(regexp))
                return true;
        }
        if (flags & MatchSymbolName) {
            const QByteArray full = path + node->symbolName;
            if (!match.isEmpty() && full.contains(match))
                return true;
            if (flags & MatchRegExp && QString::fromLocal8Bit(full).contains(regexp))
                return true;
        }
        return false;
    }
    const QRegExp &regexp;
    const QByteArray &match;
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


Daemon::Daemon(QObject *parent)
    : QObject(parent)
{
    qRegisterMetaType<Path>("Path");
    qRegisterMetaType<CXTranslationUnit>("CXTranslationUnit");
    connect(&mParseThread, SIGNAL(fileParsed(Path, void*)), &mVisitThread, SLOT(onFileParsed(Path, void*)));
    mParseThread.start();
    mVisitThread.start();
    mFileManager.start();
}

Daemon::~Daemon()
{
    mParseThread.abort();
    mVisitThread.quit();
    mFileManager.quit();
    QThread *threads[] = { &mParseThread, &mVisitThread, &mFileManager, 0 };
    for (int i=0; threads[i]; ++i)
        threads[i]->wait();
}

bool Daemon::start()
{
    if (!mEbus.start())
        return false;
    connect(&mEbus, SIGNAL(ebusConnected(EBus*)), this, SLOT(ebusConnected(EBus*)));
    return true;
}

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

static QHash<QByteArray, QVariant> syntax()
{
    return createResultMap("Syntax: rtags --command=command [--argument1, --argument2=foo, ...]\n"
                           "commands: syntax|quit|add|remove|lookupline|makefile|daemonize|files|lookup\n");
}

QHash<QByteArray, QVariant> Daemon::runCommand(const QHash<QByteArray, QVariant> &dashArgs,
                                               const QList<QByteArray>& freeArgs)
{
    qDebug() << "runCommand" << dashArgs << freeArgs;
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
    } else if (cmd == "printtree") {
        return printTree(dashArgs, freeArgs);
    } else if (cmd == "followsymbol") {
        return followSymbol(dashArgs, freeArgs);
    } else if (cmd == "makefile") {
        return addMakefile(dashArgs, freeArgs);
    } else if (cmd == "lookup") {
        return lookup(dashArgs, freeArgs);
    } else if (cmd == "load") {
        return load(dashArgs, freeArgs);
    } else if (cmd == "temporaryfile") {
        return addTemporaryFile(dashArgs, freeArgs);
    }
    return createResultMap("Unknown command");
}

QHash<QByteArray, QVariant> Daemon::addMakefile(const QHash<QByteArray, QVariant>& dashArgs,
                                                const QList<QByteArray>& freeArgs)
{
    Q_UNUSED(dashArgs);

    if (freeArgs.isEmpty())
        return createResultMap("No Makefile passed");

    Path makefile = freeArgs.first();
    if (!makefile.isResolved())
        makefile.resolve();
    if (!makefile.isFile()) {
        return createResultMap("Makefile does not exist: " + makefile);
    }
    mFileManager.addMakefile(makefile);
    return createResultMap("Added makefile");
}

QHash<QByteArray, QVariant> Daemon::addTemporaryFile(const QHash<QByteArray, QVariant>& dashArgs,
                                                     const QList<QByteArray>& freeArgs)
{
    if (freeArgs.isEmpty())
        return createResultMap("No temporary file specified");

    QByteArray filename = freeArgs.first();

    if (dashArgs.contains("remove")) {
        if (TemporaryFiles::instance()->removeFile(filename))
            return createResultMap("Temporary file removed");
        else
            return createResultMap("Temporary file does not exist: " + filename);
    }

    QByteArray content;
    QFile stdinfile;
    stdinfile.open(STDIN_FILENO, QFile::ReadOnly);
    while (!stdinfile.atEnd())
        content += stdinfile.read(8192);

    TemporaryFiles::instance()->addFile(filename, content);

    return createResultMap("Temporary file added");
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

    QRegExp rx;
    QByteArray ba;
    uint flags = 0;
    if (args.contains("regexp")) {
        rx = QRegExp(QString::fromLocal8Bit(freeArgs.value(0)));
        if (!rx.isEmpty() && rx.isValid())
            flags |= MatchBase::MatchRegExp;
    } else {
        ba = freeArgs.value(0);
    }
    if (args.contains("symbolonly"))
        flags |= MatchBase::SymbolOnly;

    if (args.contains("filename"))
        flags |= MatchBase::MatchFileNames;
    if (args.contains("symbolname") || !(flags & (MatchBase::MatchFileNames)))
        flags |= MatchBase::MatchSymbolName;

    GenericMatch match(nodeTypes, flags, rx, ba);
    mVisitThread.lookup(&match);

    return createResultMap(match.output);
}

QHash<QByteArray, QVariant> Daemon::load(const QHash<QByteArray, QVariant>&,
                                         const QList<QByteArray> &freeArgs)
{
    int added = 0;
    foreach(const QByteArray &arg, freeArgs) {
        Path filename = arg;
        if (!filename.isResolved())
            filename.resolve();
        if (!filename.isFile()) {
            qWarning("Can't find %s", arg.constData());
        } else {
            mVisitThread.invalidate(filename);
            mParseThread.load(filename, mFileManager.arguments(filename));
            ++added;
        }
    }
    return createResultMap("Loading " + QByteArray::number(added) + " files");
}

QHash<QByteArray, QVariant> Daemon::followSymbol(const QHash<QByteArray, QVariant>& args,
                                                 const QList<QByteArray> &freeArgs)
{
    if (freeArgs.size() != 1)
        return createResultMap("Invalid args");
    Path path = freeArgs.first();
    if (!path.resolve())
        return createResultMap("Invalid file " + freeArgs.first());
    bool ok;
    const int line = args.value("line").toUInt(&ok);
    if (!ok)
        return createResultMap("Invalid line arg");
    const int col = args.value("column").toUInt(&ok);
    if (!ok)
        return createResultMap("Invalid column arg");
    const Location loc(path, line, col);
    FollowSymbolMatch match(loc);
    mVisitThread.lookup(&match);
    if (match.found) {
        return createResultMap(match.found->location.toString());
    } else {
        return createResultMap("Can't follow symbol");
    }
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


QHash<QByteArray, QVariant> Daemon::printTree(const QHash<QByteArray, QVariant>&, const QList<QByteArray> &)
{
    struct TreeMatch : public Match
    {
        TreeMatch()
            : Match(Node::All)
        {}
        QByteArray out;
        virtual MatchResult match(const QByteArray &, const Node *node)
        {
            out += node->toString() + '\n';
            return Recurse;
        }
    } match;
    mVisitThread.lookup(&match);
    return createResultMap(match.out);
}

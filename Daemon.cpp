#include "Daemon.h"
#ifndef EBUS_ENABLED
#include "DaemonAdaptor.h"
#endif
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "ClangRunnable.h"
#include "PreCompile.h"

// const unsigned defaultFlags = (CXTranslationUnit_PrecompiledPreamble
//                                |CXTranslationUnit_CXXPrecompiledPreamble
//                                |CXTranslationUnit_CXXChainedPCH);
const unsigned defaultFlags = 0;

// ### might be worth optimizing
static uint qHash(const CXCursor &c, const Location &l)
{
    QByteArray u = eatString(clang_getCursorUSR(c));
    u.reserve(u.size() + 32);
    u += char(clang_getCursorKind(c)); // ### is this guaranteed to fit in a byte?
    u += l.fileName;
    u += QByteArray::number(l.line);
    u += QByteArray::number(l.column);
    return qHash(u);
}

Node::Node()
    : parent(0), nextSibling(0), firstChild(0), type(Root), hash(0)
{}

Node::Node(Node *p, CXCursor c, const Location &l, uint h)
    : parent(p), nextSibling(0), firstChild(0), location(l), hash(h)
{
    const CXCursorKind kind = clang_getCursorKind(c);
    switch (kind) {
    case CXCursor_StructDecl:
        Q_ASSERT(clang_isCursorDefinition(c));
        type = Struct;
        break;
    case CXCursor_ClassDecl:
        Q_ASSERT(clang_isCursorDefinition(c));
        type = Class;
        break;
    case CXCursor_CallExpr:
        type = MethodReference;
        symbolName = p->symbolName;
        break;
    case CXCursor_FieldDecl:
        type = VariableDeclaration;
        break;
    case CXCursor_MemberRef:
        type = VariableReference;
        symbolName = p->symbolName;
        break;
    case CXCursor_CXXMethod:
    case CXCursor_FunctionDecl:
    case CXCursor_Constructor:
    case CXCursor_Destructor:
    case CXCursor_FunctionTemplate:
    case CXCursor_ConversionFunction:
        type = clang_isCursorDefinition(c) ? MethodDefinition : MethodDeclaration;
        break;
    case CXCursor_Namespace:
        type = Namespace;
        break;
        type = Namespace;
        break;
    case CXCursor_EnumDecl:
        type = Enum;
        break;
    case CXCursor_EnumConstantDecl:
        type = EnumValue;
        break;
    default:
        qDebug() << c << l << kindToString(clang_getCursorKind(c));
        Q_ASSERT(0 && "Can't find type for this cursor");
        break;
    }
    if (symbolName.isEmpty())
        symbolName = eatString(clang_getCursorDisplayName(c));
    if (parent) {
        nextSibling = parent->firstChild;
        parent->firstChild = this;
    }
}


Node::~Node()
{
    while (firstChild) {
        Node *n = firstChild;
        firstChild = firstChild->nextSibling;
        delete n;
    }
}

QByteArray Node::toString() const
{
    if (type == Root)
        return "Root";
    int indent = 0;
    for (Node *p=parent; p; p = p->parent) {
        indent += 2;
    }
    QByteArray buf(indent, ' ');
    buf += typeToName(type);
    buf += ' ';
    buf += symbolName;
    buf += " [";
    buf += location.fileName;
    buf += ':';
    buf += QByteArray::number(location.line);
    buf += ':';
    buf += QByteArray::number(location.column);
    buf += ']';
    return buf;
}

void Node::print() const
{
    printf("%s\n", toString().constData());
    Node *child = firstChild;
    while (child) {
        child->print();
        child = child->nextSibling;
    }
}

const char *Node::typeToName(Type type, bool abbrev)
{
    switch (type) {
    case Enum: return abbrev ? "e" : "Enum";
    case EnumValue: return abbrev ? "ev" : "EnumValue";
    case Root: return abbrev ? "r" : "Root";
    case MethodDeclaration: return abbrev ? "ml" : "MethodDeclaration";
    case MethodDefinition: return abbrev ? "md" : "MethodDefinition";
    case Class: return abbrev ? "c" : "Class";
    case Struct: return abbrev ? "s" : "Struct";
    case MethodReference: return abbrev ? "mr" : "MethodReference";
    case Namespace: return abbrev ? "n" : "Namespace";
    case VariableDeclaration: return abbrev ? "vd" : "VariableDeclaration";
    case VariableReference: return abbrev ? "vr" : "VariableReference";
    case None:
    case All:
        break;
    }
    Q_ASSERT(0 && "Invalid type");
    return "Invalid";
}

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
    QByteArray fileName = eatString(clang_getFileName(file));
    resolvePath(fileName);
    if (fileExists(fileName)) {
        text += QString(", %1:%2:%3").arg(QString::fromLocal8Bit(fileName).split('/', QString::SkipEmptyParts).last()).arg(line).arg(column);
    }
    if (clang_isCursorDefinition(cursor))
        text += ", def";
    dbg << text;
    return dbg;
}


Daemon::Daemon(QObject *parent)
    : QObject(parent), m_index(clang_createIndex(1, 0)), m_root(new Node), m_pendingTranslationUnits(0)
#ifdef EBUS_ENABLED
    , m_server(0)
#endif
{
    // ### for now
    PreCompile::setPath("/tmp");

    // Database::init("database.db"); // needs to be added for each command likely
    FUNC1(parent);
    connect(&m_fileSystemWatcher, SIGNAL(fileChanged(QString)), this, SLOT(onFileChanged(QString)));
    m_threadPool.setExpiryTimeout(60000); // ### is this enough?
}

Daemon::~Daemon()
{
    FUNC;
    clang_disposeIndex(m_index);
    delete m_root;
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
    m_server = new QTcpServer(this);
    connect(m_server, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    if (!m_server->listen(QHostAddress::LocalHost, EBus::port())) {
        delete m_server;
        m_server = 0;
        return false;
    }
    return true;
#endif
}

static QHash<QByteArray, QVariant> syntax()
{
    FUNC;
    return createResultMap("Syntax: rtags --command=command [--argument1, --argument2=foo, ...]\n"
                           "commands: syntax|quit|add|remove|lookupline|makefile|daemonize|files|lookup\n");
}

void Daemon::onFileChanged(const QString &p)
{
    qDebug() << p;
    FUNC1(p);
    QByteArray path = p.toLocal8Bit();
    const QSet<QByteArray> deps = m_dependencies.value(path);
    foreach(const QByteArray &dep, deps)
        removeReferences(dep);
    removeReferences(path);
    if (m_files.contains(path))
        reparseFile(path);
    foreach(const QByteArray &dep, deps)
        reparseFile(dep);
    if (m_files.contains(path) && !fileExists(path)) {
        // ### this needs some love
        QHash<QByteArray, QVariant> args;
        args.insert("file", path);
        removeSourceFile(args);
    }
}

QHash<QByteArray, QVariant> Daemon::runCommand(const QHash<QByteArray, QVariant> &args)
{
    FUNC1(args);

    QString cmd = args.value("command").toString();
    if (cmd.isEmpty())
        return createResultMap("No command or path specified");

    if (cmd == "syntax") {
        return syntax();
    } else if (cmd == "quit") {
        ClangRunnable::abort();
        QTimer::singleShot(100, QCoreApplication::instance(), SLOT(quit()));
        // hack to make the quit command properly respond before the server goes down
        return createResultMap("quitting");
    } else if (cmd == "add") {
        return addSourceFile(args);
    } else if (cmd == "remove") {
        return removeSourceFile(args);
    } else if (cmd == "printtree") {
        m_root->print();
        return createResultMap("Done");;
    } else if (cmd == "lookupline") {
        return lookupLine(args);
    } else if (cmd == "makefile") {
        return addMakefile(args);
    } else if (cmd == "files") {
        return fileList(args);
    } else if (cmd == "lookup") {
        return lookup(args);
    } else if (cmd == "saveast") {
        return saveAST(args);
    } else if (cmd == "loadast") {
        return loadAST(args);
    }
    return createResultMap("Unknown command");
}

static bool contains(const QByteArray &key, const QRegExp &rx)
{
    return QString::fromLocal8Bit(key).contains(rx);
}

static bool contains(const QByteArray &key, const QByteArray &b)
{
    return key.contains(b);
}

template <typename T>
static QList<QByteArray> matches(const QHash<QByteArray, GccArguments> &translationUnits,
                                 const T &t)
{
    FUNC2(translationUnits, t);
    // use QStringBuilder???
    QList<QByteArray> matches;
    QHash<QByteArray, GccArguments>::const_iterator it = translationUnits.begin();
    while (it != translationUnits.end()) {
        const QByteArray &key = it.key();
        if (contains(key, t))
            matches += key;
        ++it;
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
    QList<QByteArray> out;
    if (pattern.isEmpty()) {
        out = m_files.keys();
    } else if (regexp) {
        QRegExp rx(pattern);
        out = matches(m_files, rx);
    } else {
        out = matches(m_files, pattern);
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

bool Daemon::addSourceFile(const QByteArray &absoluteFilePath, unsigned options, QHash<QByteArray, QVariant> *result)
{
    FUNC2(absoluteFilePath, options);
    if (!fileExists(absoluteFilePath)) {
        if (result)
            result->insert("result", "File doesn't exist");
        return false;
    }
    if (m_files.contains(absoluteFilePath))
        return false;
    addTranslationUnit(absoluteFilePath, GccArguments(), options);
    if (result)
        result->insert("result", "Added");
    return true;
}

QHash<QByteArray, QVariant> Daemon::addSourceFile(const QHash<QByteArray, QVariant> &args)
{
    FUNC1(args);

    QByteArray file = args.value("file").toByteArray();
    if (file.isEmpty())
        return createResultMap("No file to add (use --file=<file>)");
    resolvePath(file);
    if (!fileExists(file))
        return createResultMap(file + " Doesn't exist");
    unsigned options = 0;
    for (int i = 1; i < args.size(); ++i) {
        if (args.contains("incomplete"))
            options |= CXTranslationUnit_Incomplete;
        if (args.contains("cachecompletion"))
            options |= CXTranslationUnit_CacheCompletionResults;
    }
    QHash<QByteArray, QVariant> result;
    addSourceFile(file, options, &result);
    return result;
}

bool Daemon::addMakefileLine(const QByteArray &makeLine, const QByteArray &dirpath, QSet<QByteArray> &seen)
{
#ifdef Q_OS_UNIX
    Q_ASSERT(dirpath.startsWith('/')); // this path should be absolute and verifie
#endif
    // qDebug() << "adding makefile line" << makeLine;
    FUNC1(makeLine);
    // ### this should be improved with quote support
    // ### move this code into GccArguments
    GccArguments args;
    if (!args.parse(makeLine, dirpath) || !args.hasInput() || !args.isCompile()) {
        if (!args.errorString().isEmpty())
            qWarning("Can't parse line %s (%s)", makeLine.constData(), qPrintable(args.errorString()));
        return false;
    }

    foreach(QByteArray filename, args.input()) {
        if (!resolvePath(filename)) {
            qWarning("%s doesn't exist", filename.constData());
            return false;
        }
        if (seen.contains(filename))
            continue;
        seen.insert(filename);
        // qDebug() << "parsing" << absoluteFilePath << defines << includes;
        addTranslationUnit(filename, args, defaultFlags);
        // printf("Done %s\n", qPrintable(absoluteFilePath));
    }
    return true;
}

QHash<QByteArray, QVariant> Daemon::addMakefile(const QHash<QByteArray, QVariant> &args)
{
    FUNC1(args);
    if (args.isEmpty())
        return createResultMap("No Makefile to add");

    QByteArray filename = args.value("file").toByteArray();
    if (filename.isEmpty())
        filename = "Makefile";
    if (!resolvePath(filename)) {
        return createResultMap("Makefile does not exist " + filename);
    }

    const QByteArray dirname = dirName(filename);
    QDir::setCurrent(dirname);
    const char *basename = filename.constData() + dirname.size() + 1;
    QProcess proc;
    proc.start(QLatin1String("make"),
               QStringList()
               << QLatin1String("-B")
               << QLatin1String("-n")
               << QLatin1String("-f")
               << QLatin1String(basename));
    if (!proc.waitForFinished(-1)) {
        return createResultMap("Unable to wait for make finish");
    }
    if (proc.exitCode() != 0) {
        return createResultMap("Make returned error: " + proc.readAllStandardError());
    }

    QSet<QByteArray> seen;
    QByteArray error;
    QList<QByteArray> makeData = proc.readAllStandardOutput().split('\n');
    QRegExp accept(args.value("accept").toString());
    QRegExp reject(args.value("reject").toString());
    int count = 0;
    foreach(const QByteArray& makeLine, makeData) {
        if (makeLine.isEmpty()) {
            continue;
        }
        if (reject.isValid() && !reject.isEmpty() && QString::fromLocal8Bit(makeLine).contains(reject)) {
            if (Options::s_verbose) 
                qDebug() << "rejecting" << makeLine << reject.pattern();
            continue;
        }
        if (accept.isValid() && !accept.isEmpty() && !QString::fromLocal8Bit(makeLine).contains(accept)) {
            if (Options::s_verbose) 
                qDebug() << "not accepting" << makeLine << accept.pattern();
            continue;
        }

        if (addMakefileLine(makeLine, dirname, seen))
            ++count;
    }

    if (!error.isEmpty()) // ### createErrorMap()?
        return createResultMap(error);
    return createResultMap("Added " + QByteArray::number(count) + " translation units");
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

static bool isValidCursor(CXCursor cursor)
{
    FUNC;
    CXCursorKind kind = clang_getCursorKind(cursor);
    return !clang_isInvalid(kind);
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

static inline QString cursorData(CXCursor cursor)
{
    CXString a = clang_getCursorDisplayName(cursor);
    QString ret = path(cursor) + clang_getCString(a);
    ret += ' ';
    ret += kindToString(clang_getCursorKind(cursor));
    clang_disposeString(a);
    if (clang_isDeclaration(clang_getCursorKind(cursor))) {
        ret += " decl";
    }

    return ret;
}

enum Type {
    DeclarationName = 'd',
    DefinitionName = 'f',
    ReferenceName = 'r',
};

static inline bool modifiesPath(const Node *node)
{
    switch (node->type) {
    case Node::Namespace:
    case Node::Class:
    case Node::Struct:
        return true;
    default:
        break;
    }
    return false;
}

enum { ExactMatch = 0x100000 };
static int add(QByteArray &results, const QByteArray &symbol, QByteArray path, char *buffer, int bufferLength, const Node *node, uint flags)
{
    Q_ASSERT(node);
    const bool exactMatch = (flags & ExactMatch);
    if (exactMatch && !path.isEmpty() && !symbol.startsWith(path) && modifiesPath(node->parent))
        return 0;
    int ret = 0;
    if (node->type & flags) {
        const QByteArray full = path + node->symbolName;
        // ### this could maybe be optimized to reuse the same buffer again and again
        if (exactMatch) {
            if (symbol == full)
                ++ret;
        } else if (full.contains(symbol)) {
            ++ret;
        }
        if (ret) {
            const int count = snprintf(buffer, bufferLength, "%s %s \"%s:%d:%d\"\n",
                                       Node::typeToName(node->type, true),
                                       full.constData(),
                                       node->location.fileName.constData(),
                                       node->location.line, node->location.column);
            Q_ASSERT(count < bufferLength);
            results += QByteArray::fromRawData(buffer, count);
        }
    }

    if (::modifiesPath(node)) {
        path.append(node->symbolName + "::");
    }
    // ### could consider short circuiting here if for example we know this node
    // ### only has MethodReference children and !(types & MethodReference) || !ret
    for (Node *c = node->firstChild; c; c = c->nextSibling) {
        ret += add(results, symbol, path, buffer, bufferLength, c, flags);
    }
    return ret;
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

QHash<QByteArray, QVariant> Daemon::lookup(const QHash<QByteArray, QVariant> &args)
{
    const QByteArray symbol = args.value("symbol").toByteArray();
    if (symbol.isEmpty()) 
        return createResultMap("No symbol in lookup request");

    enum { BufferLength = 512 };
    char buffer[BufferLength + 1];
    QByteArray results;
    uint flags = 0;
    foreach(const QByteArray &type, args.value("types").toByteArray().split(',')) {
        if (type.isEmpty())
            continue;
        const Node::Type t = stringToType(type);
        if (t) {
            flags |= t;
        } else {
            qWarning("Can't parse type %s", type.constData());
        }
    }
    if (!flags)
        flags = (Node::All & ~Node::Root);
    if (args.contains("exact"))
        flags |= ExactMatch;

    ::add(results, symbol, QByteArray(), buffer, BufferLength, m_root, flags);
    return createResultMap(results);
}

QHash<QByteArray, QVariant> Daemon::loadAST(const QHash<QByteArray, QVariant> &args)
{
    QByteArray filename = args.value("file").toByteArray();
    if (filename.isEmpty())
        return createResultMap("No filename specified (use --file=<filename>)");
    if (m_files.contains(filename))
        return createResultMap("File already loaded");
    QByteArray file = QCoreApplication::applicationDirPath().toLocal8Bit() + "/ast" + filename;
    if (!resolvePath(file))
        return createResultMap("AST file does not exist");
    CXTranslationUnit unit = clang_createTranslationUnit(m_index, file.constData());
    // m_files[filename] = unit;
    clang_disposeTranslationUnit(unit);
    return createResultMap("AST file loaded");
}

QHash<QByteArray, QVariant> Daemon::saveAST(const QHash<QByteArray, QVariant> &args)
{
    // QByteArray filename = args.value("file").toByteArray();
    // if (filename.isEmpty())
    //     return createResultMap("No filename specified (use --file=<filename>)");
    // QHash<QByteArray, CXTranslationUnit>::const_iterator it = m_files.find(filename);
    // if (it == m_files.end())
    //     return createResultMap("No translation unit for filename found");
    // if (writeAST(it))
    //     return createResultMap("Saved");
    // return createResultMap("Unable to save translation unit");
}

bool Daemon::writeAST(const QHash<QByteArray, CXTranslationUnit>::const_iterator &it)
{
    FUNC;
    QByteArray filename = it.key();
    CXTranslationUnit unit = it.value();

    QFileInfo finfo(QCoreApplication::applicationDirPath() + "/ast" + filename);
    QDir dir(finfo.absolutePath());
    if (!dir.exists())
        dir.mkpath(finfo.absolutePath());

    QByteArray outname = finfo.absoluteFilePath().toLocal8Bit();
    const int ret = clang_saveTranslationUnit(unit, outname.constData(),
                                              clang_defaultSaveOptions(unit));
    return (ret == 0);
}

void Daemon::addTranslationUnit(const QByteArray &absoluteFilePath,
                                const GccArguments &args,
                                unsigned options)
{
    FUNC3(absoluteFilePath, args, options);
    ++m_pendingTranslationUnits;
    m_fileSystemWatcher.addPath(absoluteFilePath);

    const QList<QByteArray> compilerOptions = args.includePaths() + args.arguments("-D");
    m_files[absoluteFilePath] = args;

    QList<QByteArray> pchoptions = compilerOptions;

    PreCompile* precompile = PreCompile::get(pchoptions);
    QString prefilename = precompile->filename();
    if (!prefilename.isEmpty())
        pchoptions << "-include-pch" << prefilename.toLocal8Bit();

    ClangRunnable *runnable = new ClangRunnable(absoluteFilePath, options, pchoptions, m_index);
    connect(runnable, SIGNAL(error(QByteArray)), this, SLOT(onParseError(QByteArray)));
    connect(runnable, SIGNAL(fileParsed(QByteArray, QList<QByteArray>, void*)),
            this, SLOT(onFileParsed(QByteArray, QList<QByteArray>, void*)));
    m_threadPool.start(runnable);
}
void Daemon::onParseError(const QByteArray &absoluteFilePath)
{
    if (!--m_pendingTranslationUnits && Options::s_verbose)
        m_root->print();
    FUNC1(absoluteFilePath);
    qWarning("Failed to add %s", absoluteFilePath.constData());
}

Node *Daemon::createOrGet(CXCursor cursor)
{
    const CXCursorKind kind = clang_getCursorKind(cursor);
    // blacklist
    switch (kind) {
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
        if (clang_isCursorDefinition(cursor))
            break;
        // forward declaration, fall through
    case CXCursor_FirstStmt:
    case CXCursor_FirstExpr:
    case CXCursor_UnexposedDecl:
    case CXCursor_TypedefDecl:
    case CXCursor_TypeRef:
    case CXCursor_VarDecl:
    case CXCursor_DeclRefExpr:
    case CXCursor_MemberRefExpr:
    case CXCursor_ParmDecl:
    case CXCursor_UsingDirective:
    case CXCursor_NamespaceRef:
    case CXCursor_TemplateTypeParameter:
    case CXCursor_OverloadedDeclRef:
    case CXCursor_CXXBaseSpecifier:
    case CXCursor_ClassTemplate:
    case CXCursor_NonTypeTemplateParameter:
    case CXCursor_TemplateRef:
    case CXCursor_UnionDecl:
    case CXCursor_ClassTemplatePartialSpecialization:
    case CXCursor_LabelStmt:
    case CXCursor_LabelRef:
    case CXCursor_UsingDeclaration:
    case CXCursor_TemplateTemplateParameter:
        return createOrGet(clang_getCursorSemanticParent(cursor));
    case CXCursor_TranslationUnit:
        return m_root;
    default:
        break;
    }
    if (clang_isInvalid(kind))
        return m_root;
    const Location location(cursor);
    if (!location.exists())
        return createOrGet(clang_getCursorSemanticParent(cursor));

    const uint hash = qHash(cursor, location);
    if (kind == CXCursor_CallExpr || kind == CXCursor_MemberRef) {
        if (m_nodes.contains(hash))
            return m_nodes.value(hash);
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (isValidCursor(ref) && !m_pendingReferences.contains(hash)) {
            const PendingReference p = { cursor, ref, location };
            m_pendingReferences[hash] = p;
            Q_ASSERT(!clang_equalCursors(ref, cursor));
        }
        return createOrGet(clang_getCursorSemanticParent(cursor));
    }

    Node *&node = m_nodes[hash];
    if (!node) {
        node = new Node(createOrGet(clang_getCursorSemanticParent(cursor)), cursor, location, hash);
        Q_ASSERT(node->parent);
    }
    return node;
}

CXChildVisitResult Daemon::buildTree(CXCursor cursor, CXCursor, CXClientData data)
{
    Daemon *daemon = reinterpret_cast<Daemon*>(data);
    daemon->createOrGet(cursor);
    return CXChildVisit_Recurse;
}

struct PrecompileData
{
    QList<QByteArray> direct;
    QList<QByteArray> all;
};

static void precompileHeaders(CXFile included_file, CXSourceLocation*,
                              unsigned include_len, CXClientData client_data)
{
    if (!include_len)
        return;

    CXString filename = clang_getFileName(included_file);

    PrecompileData* data = reinterpret_cast<PrecompileData*>(client_data);
    QByteArray rfn = clang_getCString(filename);
    if (include_len == 1)
        data->direct.append(rfn);
    data->all.append(rfn);
    clang_disposeString(filename);
}

void Daemon::onFileParsed(const QByteArray &absoluteFilePath, const QList<QByteArray> &options, void *u)
{
    FUNC2(absoluteFilePath, u);
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    PrecompileData pre;
    clang_getInclusions(unit, precompileHeaders, &pre);
    foreach(const QByteArray &header, pre.all) {
        if (!m_dependencies.contains(header))
            m_fileSystemWatcher.addPath(header);
        m_dependencies[header].insert(absoluteFilePath);
    }
    PreCompile* precompile = PreCompile::get(options);
    precompile->add(pre.direct, pre.all);

    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    clang_visitChildren(cursor, buildTree, this);
    for (QHash<uint, PendingReference>::const_iterator it = m_pendingReferences.begin();
         it != m_pendingReferences.end(); ++it) {
        const PendingReference &p = it.value();
        Q_ASSERT(!m_nodes.contains(it.key()));
        m_nodes[it.key()] = new Node(createOrGet(p.reference), p.cursor, p.location, it.key());
    }
    m_pendingReferences.clear();
    clang_disposeTranslationUnit(unit);
    qDebug() << m_nodes.size();
    if (!--m_pendingTranslationUnits && Options::s_verbose)
        m_root->print();

    // m_threadPool.start(new ProcessFileRunnable(absoluteFilePath, unit));
}

void Daemon::reparseFile(const QByteArray &absoluteFilePath)
{
    m_fileSystemWatcher.removePath(absoluteFilePath);
    addTranslationUnit(absoluteFilePath, m_files.value(absoluteFilePath), defaultFlags);
}

int count(Node *node)
{
    int ret = 1;
    for (Node *c = node->firstChild; c; c = c->nextSibling)
        ret += count(c);
    return ret;
}

static int recursiveDelete(Node *node, QHash<unsigned, Node*> &nodes)
{
    int ret = 1;
    for (Node *c = node->firstChild; c; c = c->nextSibling)
        ret += recursiveDelete(c, nodes);

    node->firstChild = 0;
    Q_ASSERT(nodes.contains(node->hash));
    nodes.remove(node->hash);
    delete node;
    return ret;
}

static int removeChildren(Node *node, const QByteArray &absoluteFilePath, QHash<unsigned, Node*> &nodes)
{
    Node *prev = 0;
    Node *child = node->firstChild;
    int ret = 0;
    while (child) {
        if (child->location.fileName == absoluteFilePath) {
            if (!prev) {
                node->firstChild = child->nextSibling;
                ret += recursiveDelete(child, nodes);
                child = node->firstChild;
            } else {
                prev->nextSibling = child->nextSibling;
                ret += recursiveDelete(child, nodes);
                child = prev->nextSibling;
            }
        } else {
            removeChildren(child, absoluteFilePath, nodes);
            prev = child;
            child = child->nextSibling;
        }
    }
    return ret;
}

void Daemon::removeReferences(const QByteArray &absoluteFilePath)
{
    int count = removeChildren(m_root, absoluteFilePath, m_nodes);
    qDebug("Removed %d nodes %d", count, m_nodes.size());
}

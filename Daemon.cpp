#include "Daemon.h"
#ifndef EBUS_ENABLED
#include "DaemonAdaptor.h"
#endif
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "ClangJob.h"
#include "Database.h"

const QByteArray dirName(const QByteArray &filePath)
{
    Q_ASSERT(!filePath.endsWith('/'));
    return filePath.left(filePath.lastIndexOf('/'));
}

static QVariantMap createResultMap(const QByteArray& result)
{
    QVariantMap ret;
    ret.insert(QLatin1String("result"), result);
    return ret;
}

static inline QDebug operator<<(QDebug dbg, CXCursor cursor)
{
    QString text = "CXCursor(";
    if (clang_isInvalid(clang_getCursorKind(cursor))) {
        text += ")";
        dbg << text;
        return dbg;
    }

    QByteArray name = eatString(clang_getCursorSpelling(cursor));
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
        text += QString(", %1:%2:%3").arg(QString::fromLocal8Bit(fileName)).arg(line).arg(column);
    }
    text += ")";
    dbg << text;
    return dbg;
}


Daemon::Daemon(QObject *parent)
    : QObject(parent), m_index(clang_createIndex(1, 0))
#ifdef EBUS_ENABLED
    , m_server(0)
#endif
{
    m_threadPool.init(8); // ### configurable?
    // Database::init("database.db"); // needs to be added for each command likely
    FUNC1(parent);
    connect(&m_fileSystemWatcher, SIGNAL(fileChanged(QString)), this, SLOT(onFileChanged(QString)));
}

Daemon::~Daemon()
{
    FUNC;
    foreach(CXTranslationUnit unit, m_translationUnits) {
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(m_index);
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

static QVariantMap syntax()
{
    FUNC;
    return createResultMap("Syntax: rtags --command=command [--argument1, --argument2=foo, ...]\n"
                           "commands: syntax|quit|add|remove|lookupline|makefile|daemonize|files|lookup\n");
}

void Daemon::onFileChanged(const QString &p)
{
    FUNC1(p);
    QByteArray path = p.toLocal8Bit();
    resolvePath(path);
    m_fileSystemWatcher.removePath(p);
    if (fileExists(path)) {
        addSourceFile(path);
    } else {
        QVariantMap args;
        args.insert(QLatin1String("file"), path);
        removeSourceFile(args);
    }
}

QVariantMap Daemon::runCommand(const QVariantMap &args)
{
    FUNC1(args);

    QString cmd = args.value(QLatin1String("command")).toString();
    QByteArray path = args.value(QLatin1String("currentpath")).toByteArray();
    if (path.isEmpty() || cmd.isEmpty())
        return createResultMap("No command or path specified");

    if (cmd == "syntax") {
        return syntax();
    } else if (cmd == "quit") {
        QTimer::singleShot(100, QCoreApplication::instance(), SLOT(quit()));
        // hack to make the quit command properly respond before the server goes down
        return createResultMap("quitting");
    } else if (cmd == "add") {
        return addSourceFile(args);
    } else if (cmd == "remove") {
        return removeSourceFile(args);
    } else if (cmd == "lookupline") {
        return lookupLine(args);
    } else if (cmd == "makefile") {
        return addMakefile(path, args);
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
static QList<QByteArray> matches(const QHash<QByteArray, CXTranslationUnit> &translationUnits,
                                 const T &t)
{
    FUNC2(translationUnits, t);
    // use QStringBuilder???
    QList<QByteArray> matches;
    QHash<QByteArray, CXTranslationUnit>::const_iterator it = translationUnits.begin();
    while (it != translationUnits.end()) {
        const QByteArray &key = it.key();
        if (contains(key, t))
            matches += key;
        ++it;
    }
    return matches;
}

QVariantMap Daemon::fileList(const QVariantMap &args)
{
    FUNC1(args);
    bool regexp = true;
    QByteArray pattern;
    if (pattern.isEmpty())
        pattern = args.value(QLatin1String("regexp")).toByteArray();
    if (pattern.isEmpty()) {
        pattern = args.value(QLatin1String("match")).toByteArray();
        regexp = false;
    }
    QList<QByteArray> out;
    if (pattern.isEmpty()) {
        out = m_translationUnits.keys();
    } else if (regexp) {
        QRegExp rx(pattern);
        out = matches(m_translationUnits, rx);
    } else {
        out = matches(m_translationUnits, pattern);
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

bool Daemon::addSourceFile(const QByteArray &absoluteFilePath, unsigned options, QVariantMap *result)
{
    FUNC2(absoluteFilePath, options);
    if (!fileExists(absoluteFilePath)) {
        if (result)
            result->insert(QLatin1String("result"), QLatin1String("File doesn't exist"));
        return false;
    }
    CXTranslationUnit unit = m_translationUnits.take(absoluteFilePath);
    QList<QByteArray> compilerOptions;
    if (unit) {
        clang_disposeTranslationUnit(unit);
        compilerOptions = Database::takeCompilerOptions(absoluteFilePath);
    }
    addTranslationUnit(absoluteFilePath, options, compilerOptions);
    if (result)
        result->insert(QLatin1String("result"), QLatin1String("Added"));
    return true;
}

QVariantMap Daemon::addSourceFile(const QVariantMap &args)
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
    QVariantMap result;
    addSourceFile(file, options, &result);
    return result;
}

bool Daemon::addMakefileLine(const QList<QByteArray> &line)
{
    FUNC1(line);
    GccArguments args;
    if (!args.parse(line) || !args.hasInput()) {
        QByteArray joined;
        foreach(const QByteArray &l, line) {
            joined += l + ' ';
        }
        joined.chop(1);
        qWarning("Can't parse line %s [%s]", joined.constData(), qPrintable(args.errorString()));
        return false;
    }

    if (!args.isCompile()) { // Just accept link lines without doing anything
        return true;
    }

    const QList<QByteArray> options = args.arguments("-I") + args.arguments("-D");

    foreach(QByteArray filename, args.input()) {
        resolvePath(filename);
        if (!fileExists(filename)) {
            qWarning("%s doesn't exist", filename.constData());
            return false;
        }
        QHash<QByteArray, CXTranslationUnit>::iterator it = m_translationUnits.find(filename);
        if (it != m_translationUnits.end()) {
            // ### need to check if it was parsed with the same flags, and if so reparse
            clang_disposeTranslationUnit(it.value());
            m_translationUnits.erase(it);
        }

        const unsigned defaultFlags = (CXTranslationUnit_PrecompiledPreamble
                                       |CXTranslationUnit_CXXPrecompiledPreamble
                                       |CXTranslationUnit_CXXChainedPCH);

        // qDebug() << "parsing" << absoluteFilePath << defines << includes;
        addTranslationUnit(filename,
                           defaultFlags,
                           options);
        // printf("Done %s\n", qPrintable(absoluteFilePath));
    }

    return true;
}

QVariantMap Daemon::addMakefile(const QByteArray& path, const QVariantMap &args)
{
    FUNC2(path, args);
    if (path.isEmpty() || args.isEmpty())
        return createResultMap("No Makefile to add");

    QString cwd = QDir::currentPath();
    QDir::setCurrent(path);

    QByteArray filename = args.value(QLatin1String("file")).toByteArray();
    if (filename.isEmpty())
        filename = "Makefile";
    resolvePath(filename);
    if (!fileExists(filename)) {
        QDir::setCurrent(cwd);
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
        QDir::setCurrent(cwd);
        return createResultMap("Unable to wait for make finish");
    }
    if (proc.exitCode() != 0) {
        QDir::setCurrent(cwd);
        return createResultMap("Make returned error: " + proc.readAllStandardError());
    }

    QByteArray error;
    QList<QByteArray> makeData = proc.readAllStandardOutput().split('\n');
    QRegExp accept(args.value("accept").toString());
    QRegExp reject(args.value("reject").toString());
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
        
        // ### this should be improved with quote support
        QList<QByteArray> lineOpts = makeLine.split(' ');
        const QByteArray& first = lineOpts.first();
        if ((first.contains("gcc") || first.contains("g++"))
            && !addMakefileLine(lineOpts)) {
            error += QLatin1String("Unable to add ") + makeLine + QLatin1String("\n");
        }
    }

    QDir::setCurrent(cwd);

    if (!error.isEmpty()) // ### createErrorMap()?
        return createResultMap(error);
    return createResultMap("Added " + QByteArray::number(m_translationUnits.size()) + " translation units");
}

QVariantMap Daemon::removeSourceFile(const QVariantMap &args)
{
    FUNC1(args);
    bool regexp = true;
    QByteArray pattern = args.value(QLatin1String("regexp")).toByteArray();
    if (pattern.isEmpty()) {
        pattern = args.value(QLatin1String("file")).toByteArray();
        regexp = false;
    }

    // ### need to use regexp and match partial and all that good stuff. Maybe
    // ### make it use the same code path as fileList
    if (pattern.isEmpty())
        return createResultMap("No file to remove (use --file=<filename> or --regexp=.*file.*");
    QHash<QByteArray, CXTranslationUnit>::iterator it = m_translationUnits.find(pattern);
    if (it == m_translationUnits.end())
        return createResultMap("No matches for " + pattern);
    clang_disposeTranslationUnit(it.value());
    m_fileSystemWatcher.removePath(it.key());
    m_translationUnits.erase(it);

    return createResultMap("Removed");
}

static bool isValidCursor(CXCursor cursor)
{
    FUNC;
    CXCursorKind kind = clang_getCursorKind(cursor);
    return !clang_isInvalid(kind);
}

QVariantMap Daemon::lookupLine(const QVariantMap &args)
{
    FUNC1(args);
    if (!args.contains(QLatin1String("line"))
        || !args.contains(QLatin1String("line"))
        || !args.contains(QLatin1String("column")))
        return createResultMap("Invalid argument count");

    QByteArray filename = args.value(QLatin1String("file")).toByteArray();
    int line = args.value(QLatin1String("line")).toInt();
    int column = args.value(QLatin1String("column")).toInt();

    if (filename.isEmpty() || line == 0 || column == 0)
        return createResultMap("Invalid argument type");

    if (!m_translationUnits.contains(filename))
        return createResultMap("Translation unit not found");

    CXTranslationUnit unit = m_translationUnits.value(filename);
    CXFile file = clang_getFile(unit, filename.constData());

    CXSourceLocation location = clang_getLocation(unit, file, line, column);
    CXCursor cursor = clang_getCursor(unit, location);
    if (!isValidCursor(cursor))
        return createResultMap("Unable to get cursor for location");

    CXCursorKind kind = clang_getCursorKind(cursor);
    CXCursor referenced;
    if (kind == CXCursor_CXXMethod) // might need to add more here
        referenced = clang_getCanonicalCursor(cursor);
    else
        referenced = clang_getCursorReferenced(cursor);
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

static inline QString path(CXCursor cursor)
{
    QString path;
    bool done = false;
    CXString tmp;
    do {
        cursor = clang_getCursorLexicalParent(cursor);
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_ClassDecl:
        case CXCursor_Namespace:
            tmp = clang_getCursorDisplayName(cursor);
            path.prepend(clang_getCString(tmp) + QLatin1String("::"));
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
    Declaration = 'd',
    Definition = 'f',
    Reference = 'r'
};

static void add(QByteArray &list, char *buf, const QList<Symbol> &symbols, Type type)
{
    foreach(const Symbol &symbol, symbols) {
        Q_ASSERT(symbol.location.exists());
        const int count = snprintf(buf, 511, "%c %s \"%s:%d:%d\"\n", type,
                                   symbol.symbolName.constData(),
                                   symbol.location.fileName.constData(),
                                   symbol.location.line, symbol.location.column);
        list += QByteArray::fromRawData(buf, count);
    }
}

QVariantMap Daemon::lookup(const QVariantMap &args)
{
    const QByteArray symbol = args.value(QLatin1String("symbol")).toByteArray();
    if (symbol.isEmpty()) 
        return createResultMap("No symbol in lookup request");

    bool exactMatch = false;
    if (args.contains("exact"))
        exactMatch = true;
    char buffer[512];
    QByteArray results;
    const QStringList symbolTypes = args.value(QLatin1String("types")).toString().
        split(',', QString::SkipEmptyParts);
    // ### uglehack

    qDebug() << symbol << symbolTypes;
    // qDebug() << symbol << args << symbolTypes << Database::symbolDeclarationSize()
    //          << Database::symbolDefinitionSize() << Database::symbolReferencesSize();
    
    if (symbolTypes.isEmpty() || symbolTypes.contains("declaration")) {
        add(results, buffer, Database::lookupDeclarations(symbol, exactMatch), Declaration);
    }
    if (symbolTypes.isEmpty() || symbolTypes.contains("definition")) {
        add(results, buffer, Database::lookupDefinitions(symbol, exactMatch), Definition);
    }
    if (symbolTypes.isEmpty() || symbolTypes.contains("reference")) {
        add(results, buffer, Database::lookupReferences(symbol, exactMatch), Reference);
    }
    return createResultMap(results);
}

QVariantMap Daemon::loadAST(const QVariantMap &args)
{
    QByteArray filename = args.value(QLatin1String("file")).toByteArray();
    if (filename.isEmpty())
        return createResultMap("No filename specified (use --file=<filename>)");
    if (m_translationUnits.contains(filename))
        return createResultMap("File already loaded");
    QString prefix = QCoreApplication::applicationDirPath() + QLatin1String("/ast");
    QFileInfo finfo(prefix + filename);
    if (!finfo.exists())
        return createResultMap("AST file does not exist");
    CXTranslationUnit unit = clang_createTranslationUnit(m_index, finfo.absoluteFilePath().toLocal8Bit().constData());
    m_translationUnits[filename] = unit;
    return createResultMap("AST file loaded");
}

QVariantMap Daemon::saveAST(const QVariantMap &args)
{
    QByteArray filename = args.value(QLatin1String("file")).toByteArray();
    if (filename.isEmpty())
        return createResultMap("No filename specified (use --file=<filename>)");
    QHash<QByteArray, CXTranslationUnit>::const_iterator it = m_translationUnits.find(filename);
    if (it == m_translationUnits.end())
        return createResultMap("No translation unit for filename found");
    if (writeAST(it))
        return createResultMap("Saved");
    return createResultMap("Unable to save translation unit");
}

bool Daemon::writeAST(const QHash<QByteArray, CXTranslationUnit>::const_iterator &it)
{
    FUNC;
    QString filename = it.key();
    CXTranslationUnit unit = it.value();

    QFileInfo finfo(QCoreApplication::applicationDirPath() + QLatin1String("/ast") + filename);
    QDir dir(finfo.absolutePath());
    if (!dir.exists())
        dir.mkpath(finfo.absolutePath());

    QByteArray outname = finfo.absoluteFilePath().toLocal8Bit();
    const int ret = clang_saveTranslationUnit(unit, outname.constData(),
                                              clang_defaultSaveOptions(unit));
    return (ret == 0);
}

void Daemon::addTranslationUnit(const QByteArray &absoluteFilePath,
                                unsigned options,
                                const QList<QByteArray> &compilerOptions)
{
    FUNC3(absoluteFilePath, options, compilerOptions);
    ClangJob *job = new ClangJob(absoluteFilePath, options, compilerOptions, m_index);
    connect(job, SIGNAL(error(QByteArray)), this, SLOT(onParseError(QByteArray)));
    connect(job, SIGNAL(fileParsed(QByteArray, void*)),
            this, SLOT(onFileParsed(QByteArray, void*)));
    m_threadPool.post(job);
}
void Daemon::onParseError(const QByteArray &absoluteFilePath)
{
    FUNC1(absoluteFilePath);
    qWarning("Failed to add %s", absoluteFilePath.constData());
}

static inline QByteArray symbolName(CXCursor cursor)
{
    QByteArray name;
    bool done = false;
    forever {
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_CXXMethod:
        case CXCursor_Constructor:
            if (!name.isEmpty()) {
                qDebug() << "name was" << name << "will be set to"
                         << eatString(clang_getCursorDisplayName(cursor));
            }
            Q_ASSERT(name.isEmpty());
            name = eatString(clang_getCursorDisplayName(cursor));
            break;
        case CXCursor_ClassDecl:
        case CXCursor_Namespace:
            if (!name.isEmpty())
                name.prepend("::");
            name.prepend(eatString(clang_getCursorDisplayName(cursor)));
            break;
        default:
            if (name.isEmpty())
                name = eatString(clang_getCursorDisplayName(cursor));
            done = true;
            break;
        }
        if (done)
            break;
        cursor = clang_getCursorSemanticParent(cursor);
    }
    return name;
}

struct ProcessFileUserData {
    QByteArray fileName;
    int count;
};

static CXChildVisitResult processFile(CXCursor cursor, CXCursor, CXClientData data)
{
    ProcessFileUserData &userData = *reinterpret_cast<ProcessFileUserData*>(data);
    const CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_ClassDecl:
        if (!clang_isCursorDefinition(cursor)) {
            if (Options::s_verbose) {
                qDebug() << "dropping forward declaration of" << eatString(clang_getCursorDisplayName(cursor))
                         << __LINE__;
            }
            break;
        }
        // fallthrough
    case CXCursor_Namespace:
    case CXCursor_CXXMethod:
    case CXCursor_Constructor: {
        const Location loc(cursor);
        if (!loc.exists()) {
            if (Options::s_verbose) {
                qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                         << kindToString(clang_getCursorKind(cursor))
                         << "because of missing file" << cursor << __LINE__;
            }
            break;
        }
        const QByteArray symbol = symbolName(cursor);
        if (symbol.isEmpty()) {
            if (Options::s_verbose) {
                qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                         << kindToString(clang_getCursorKind(cursor))
                         << "because of empty symbolName" << __LINE__;
            }
            break;
        }

        if (clang_isCursorDefinition(cursor)) {
            Database::setSymbolDefinition(symbol, loc);
        } else {
            Database::setSymbolDeclaration(symbol, loc);
        }
        ++userData.count;
        break; }
    case CXCursor_CallExpr: {
        const Location callLoc(cursor);
        if (!callLoc.exists()) {
            // if (Options::s_verbose) {
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                     << kindToString(clang_getCursorKind(cursor))
                     << "because of we can't find location" << __LINE__;
            // }
            break;
        }
        CXCursor method = clang_getCursorReferenced(cursor);
        if (!isValidCursor(method)) {
            if (Options::s_verbose) {
                qDebug() << "trying stuff\n" << method << endl << cursor
                         << endl << clang_getCanonicalCursor(cursor)
                         << endl << eatString(clang_getCursorUSR(cursor));
                // method = clang_getCanonicalCursor(cursor);
                // qDebug() <
                // printf("Referenced failed trying canonical %d %s\n", isValidCursor(method),
                //        kindToString(clang_getCursorKind(method)));
            }
        }
        if (!isValidCursor(method)) {
            if (Options::s_verbose) {
                qDebug() << "dropping" << eatString(clang_getCursorDisplayName(method))
                         << kindToString(clang_getCursorKind(method))
                         << "because of invalid reference" << cursor << method << __LINE__;
            }
            break;
        }
        const QByteArray symbol = symbolName(method);
        if (symbol.isEmpty()) {
            // if (Options::s_verbose) {
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(method))
                     << kindToString(clang_getCursorKind(method))
                     << "because of empty symbolName" << __LINE__;
            // }
            break;
        }
        // ### not the most efficient way to check this
        Location methodLoc = Database::lookupDeclarations(symbol, true).value(0).location;
        if (!methodLoc.exists()) {
            methodLoc = Location(method);
            if (methodLoc.exists()) {
                Database::setSymbolDeclaration(symbol, methodLoc);
                ++userData.count;
            } else {
                if (/*Options::s_verbose && */symbol != "__va_list_tag()") {
                    qDebug() << "dropping" << symbol
                             << kindToString(clang_getCursorKind(method))
                             << "because we can't find file" << __LINE__;
                }
                break;
            }
        }

        Database::addSymbolReference(symbol, callLoc);
        ++userData.count;
        break; }
    default:
        break;
    }

    return CXChildVisit_Recurse;
}

void Daemon::onFileParsed(const QByteArray &absoluteFilePath, void *u)
{
    FUNC2(absoluteFilePath, u);
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    m_fileSystemWatcher.addPath(absoluteFilePath);
    if (m_translationUnits.contains(absoluteFilePath)) {
        qWarning("We already have this file: %s", absoluteFilePath.constData());
        clang_disposeTranslationUnit(unit);
        return;
    }
    Q_ASSERT(!m_translationUnits.contains(absoluteFilePath));
    m_translationUnits[absoluteFilePath] = unit;
    // crashes right now with some issue with autoincrement primary key on Symbol
    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    ProcessFileUserData userData;
    userData.fileName = absoluteFilePath;
    userData.count = 0;
    QElapsedTimer timer;
    timer.start();
    clang_visitChildren(cursor, processFile, &userData);
    qDebug("Added %d entries (total %d/%d/%d) for %s (%lld ms)", userData.count,
           Database::symbolDeclarationSize(), Database::symbolDefinitionSize(),
           Database::symbolReferencesSize(), absoluteFilePath.constData(), timer.elapsed());
}

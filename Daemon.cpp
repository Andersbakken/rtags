#include "Daemon.h"
#ifndef EBUS_ENABLED
#include "DaemonAdaptor.h"
#endif
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "ClangThread.h"
#include "Database.h"

static QVariantMap createResultMap(const QString& result)
{
    QVariantMap ret;
    ret.insert(QLatin1String("result"), result);
    return ret;
}

static QByteArray eatString(CXString string)
{
    const QByteArray ret = clang_getCString(string);
    clang_disposeString(string);
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
    const QByteArray fileName = eatString(clang_getFileName(file));
    if (!fileName.isEmpty()) {
        text += QString(", %1:%2(%3)").arg(QString::fromLocal8Bit(fileName)).arg(line).arg(column);
    }
    text += ")";
    dbg << text;
    return dbg;
}


#define USE_THREAD
Daemon::Daemon(QObject *parent)
    : QObject(parent), m_index(clang_createIndex(1, 0))
#ifdef EBUS_ENABLED
    , m_server(0)
#endif
{
    Database::init("database.db"); // needs to be added for each command likely
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
    return createResultMap(QLatin1String("Syntax: rtags <command> [argument1, argument2, ...]\n"
                                         "commands: syntax|quit|add|remove|lookupline|makefile|daemonize|files|lookup\n"));
}

void Daemon::onFileChanged(const QString &path)
{
    FUNC1(path);
    const QFileInfo fi(path);
    if (fi.exists()) {
        qWarning("Not reparsing since it seems to crash");
        // addSourceFile(fi);
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
    QString path = args.value(QLatin1String("currentpath")).toString();
    if (path.isEmpty() || cmd.isEmpty())
        return createResultMap(QLatin1String("No command or path specified"));

    if (cmd == QLatin1String("syntax")) {
        return syntax();
    } else if (cmd == QLatin1String("quit")) {
        QTimer::singleShot(100, QCoreApplication::instance(), SLOT(quit()));
        // hack to make the quit command properly respond before the server goes down
        return createResultMap(QLatin1String("quitting"));
    } else if (cmd == QLatin1String("add")) {
        return addSourceFile(args);
    } else if (cmd == QLatin1String("remove")) {
        return removeSourceFile(args);
    } else if (cmd == QLatin1String("lookupline")) {
        return lookupLine(args);
    } else if (cmd == QLatin1String("makefile")) {
        return addMakefile(path, args);
    } else if (cmd == QLatin1String("files")) {
        return fileList(args);
    } else if (cmd == QLatin1String("lookup")) {
        return lookup(cmd, Declaration);
    } else if (cmd == QLatin1String("saveast")) {
        return saveAST(args);
    } else if (cmd == QLatin1String("loadast")) {
        return loadAST(args);
    }
    return createResultMap(QLatin1String("Unknown command"));
}

template <typename T>
static QStringList matches(const QHash<QString, CXTranslationUnit> &translationUnits, const T &t)
{
    FUNC2(translationUnits, t);
    // use QStringBuilder???
    QStringList matches;
    QHash<QString, CXTranslationUnit>::const_iterator it = translationUnits.begin();
    while (it != translationUnits.end()) {
        const QString &key = it.key();
        if (key.contains(t))
            matches += key;
        ++it;
    }
    return matches;
}

QVariantMap Daemon::fileList(const QVariantMap &args)
{
    FUNC1(args);
    bool regexp = true;
    QString pattern = args.value(QLatin1String("r")).toString();
    if (pattern.isEmpty())
        pattern = args.value(QLatin1String("regexp")).toString();
    if (pattern.isEmpty()) {
        pattern = args.value(QLatin1String("match")).toString();
        regexp = false;
    }
    QStringList out;
    if (pattern.isEmpty()) {
        out = m_translationUnits.keys();
    } else if (regexp) {
        QRegExp rx(pattern);
        out = matches(m_translationUnits, rx);
    } else {
        out = matches(m_translationUnits, pattern);
    }
    return createResultMap(out.join(QLatin1String("\n")));
}

bool Daemon::addSourceFile(const QFileInfo &fi, unsigned options, QVariantMap *result)
{
    FUNC2(fi, options);
    if (!fi.exists()) {
        if (result)
            result->insert(QLatin1String("result"), QLatin1String("File doesn't exist"));
        return false;
    }
    const QString absoluteFilePath = fi.absoluteFilePath();
    CXTranslationUnit &unit = m_translationUnits[absoluteFilePath];
    if (unit) {
#ifdef USE_THREAD
        ClangThread *thread = new ClangThread(unit, absoluteFilePath, this);
        thread->start();
#else
        if (clang_reparseTranslationUnit(unit, 0, 0, 0) != 0) {
            if (result)
                *result = QLatin1String("Failed");
            return false;
        }
#endif
        if (result)
            result->insert(QLatin1String("result"), QLatin1String("Reparsed"));
    } else {
        addTranslationUnit(absoluteFilePath, options);
        if (result)
            result->insert(QLatin1String("result"), QLatin1String("Added"));
    }
    return true;
}

QVariantMap Daemon::addSourceFile(const QVariantMap &args)
{
    FUNC1(args);

    QString file = args.value("file").toString();
    if (file.isEmpty())
        return createResultMap(QLatin1String("No file to add (use --file=<file>)"));
    const QFileInfo finfo(file);
    unsigned options = 0;
    for (int i = 1; i < args.size(); ++i) {
        if (args.contains("incomplete"))
            options |= CXTranslationUnit_Incomplete;
        if (args.contains("cachecompletion"))
            options |= CXTranslationUnit_CacheCompletionResults;
    }
    QVariantMap result;
    addSourceFile(finfo, options, &result);
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

    foreach(const QByteArray& filename, args.input()) {
        const QFileInfo fi(QString::fromLocal8Bit(filename));
        if (!fi.exists()) {
            qWarning("%s doesn't exist", filename.constData());
            return false;
        }
        const QString absoluteFilePath = fi.absoluteFilePath();
        QHash<QString, CXTranslationUnit>::iterator it = m_translationUnits.find(absoluteFilePath);
        if (it != m_translationUnits.end()) {
            // ### need to check if it was parsed with the same flags, and if so reparse
            clang_disposeTranslationUnit(it.value());
            m_translationUnits.erase(it);
        }

        const unsigned defaultFlags = (CXTranslationUnit_PrecompiledPreamble
                                       |CXTranslationUnit_CXXPrecompiledPreamble
                                       |CXTranslationUnit_CXXChainedPCH);

        // qDebug() << "parsing" << absoluteFilePath << defines << includes;
        addTranslationUnit(absoluteFilePath,
                           defaultFlags,
                           options);
        // printf("Done %s\n", qPrintable(absoluteFilePath));
    }

    return true;
}

QVariantMap Daemon::addMakefile(const QString& path, const QVariantMap &args)
{
    FUNC2(path, args);
    if (path.isEmpty() || args.isEmpty())
        return createResultMap(QLatin1String("No Makefile to add"));

    QString cwd = QDir::currentPath();
    QDir::setCurrent(path);

    QString filename = args.value(QLatin1String("file")).toString();
    if (filename.isEmpty())
        filename = QLatin1String("Makefile");
    QFileInfo finfo(filename);
    if (!finfo.exists()) {
        QDir::setCurrent(cwd);
        return createResultMap(QLatin1String("Makefile does not exist") + filename);
    }

    QDir::setCurrent(finfo.absolutePath());

    QProcess proc;
    proc.start(QLatin1String("make"), QStringList() << QLatin1String("-B") << QLatin1String("-n") << QLatin1String("-f") << finfo.fileName());
    if (!proc.waitForFinished(-1)) {
        QDir::setCurrent(cwd);
        return createResultMap(QLatin1String("Unable to wait for make finish"));
    }
    if (proc.exitCode() != 0) {
        QDir::setCurrent(cwd);
        return createResultMap(QLatin1String("Make returned error: " + proc.readAllStandardError()));
    }

    QString error;
    QList<QByteArray> makeData = proc.readAllStandardOutput().split('\n');
    foreach(const QByteArray& makeLine, makeData) {
        if (makeLine.isEmpty())
            continue;
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
    return createResultMap(QString::fromLocal8Bit("Added %1 translation units").arg(m_translationUnits.size()));
}

QVariantMap Daemon::removeSourceFile(const QVariantMap &args)
{
    FUNC1(args);
    bool regexp = true;
    QString pattern = args.value(QLatin1String("r")).toString();
    if (pattern.isEmpty())
        pattern = args.value(QLatin1String("regexp")).toString();
    if (pattern.isEmpty()) {
        pattern = args.value(QLatin1String("file")).toString();
        regexp = false;
    }

    // ### need to use regexp and match partial and all that good stuff. Maybe
    // ### make it use the same code path as fileList
    if (pattern.isEmpty())
        return createResultMap(QLatin1String("No file to remove (use --file=<filename>"));
    QHash<QString, CXTranslationUnit>::iterator it = m_translationUnits.find(pattern);
    if (it == m_translationUnits.end())
        return createResultMap(QLatin1String("No matches for ") + pattern);
    clang_disposeTranslationUnit(it.value());
    m_fileSystemWatcher.removePath(it.key());
    m_translationUnits.erase(it);

    return createResultMap(QLatin1String("Removed"));
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
        return createResultMap(QLatin1String("Invalid argument count"));

    QString filename = args.value(QLatin1String("file")).toString();
    int line = args.value(QLatin1String("line")).toInt();
    int column = args.value(QLatin1String("column")).toInt();

    if (filename.isEmpty() || line == 0 || column == 0)
        return createResultMap(QLatin1String("Invalid argument type"));

    if (!m_translationUnits.contains(filename))
        return createResultMap(QLatin1String("Translation unit not found"));

    CXTranslationUnit unit = m_translationUnits.value(filename);
    CXFile file = clang_getFile(unit, filename.toLocal8Bit().constData());

    CXSourceLocation location = clang_getLocation(unit, file, line, column);
    CXCursor cursor = clang_getCursor(unit, location);
    if (!isValidCursor(cursor))
        return createResultMap(QLatin1String("Unable to get cursor for location"));

    CXCursorKind kind = clang_getCursorKind(cursor);
    CXCursor referenced;
    if (kind == CXCursor_CXXMethod) // might need to add more here
        referenced = clang_getCanonicalCursor(cursor);
    else
        referenced = clang_getCursorReferenced(cursor);
    if (!isValidCursor(referenced))
        return createResultMap(QLatin1String("No referenced cursor"));

    location = clang_getCursorLocation(referenced);
    unsigned int rline, rcolumn, roffset;
    CXFile rfile;
    clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
    CXString rfilename = clang_getFileName(rfile);

    QString ret = QString("Symbol (decl) at %1, line %2 column %3").
        arg(clang_getCString(rfilename)).
        arg(rline).arg(rcolumn);

    clang_disposeString(rfilename);

    return createResultMap(ret);
}

struct UserData {
    const QString &symbol;
    QStringList results;
    Daemon::LookupType type;
    int count;
};

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


static CXChildVisitResult lookupSymbol(CXCursor cursor, CXCursor, CXClientData client_data)
{
    FUNC;
    UserData *data = reinterpret_cast<UserData*>(client_data);
    ++data->count;
    CXSourceLocation location = clang_getCursorLocation(cursor);
    unsigned int line, column, offset;
    CXFile file;
    clang_getInstantiationLocation(location, &file, &line, &column, &offset);
    CXString fileName = clang_getFileName(file);
    QString ret = QString("%1:%2(%3) ").arg(clang_getCString(fileName)).arg(line).arg(column);
    clang_disposeString(fileName);
    ret += cursorData(cursor);
    if (clang_getCursorKind(cursor) == CXCursor_CallExpr) {
        CXCursor referenced = clang_getCursorReferenced(cursor);
        if (!isValidCursor(referenced))
            return CXChildVisit_Recurse;
        ret += cursorData(referenced);
    }
    qWarning("%s", qPrintable(ret));
    return CXChildVisit_Recurse;

    // if (!strcmp(clang_getCString(data->find), clang_getCString(usr))) {
    //     data->result = cursor;
    //     data->found = true;
    //     clang_disposeString(usr);
    //     return CXChildVisit_Break;
    // }
    // clang_disposeString(usr);
    return CXChildVisit_Recurse;
}

QVariantMap Daemon::lookup(const QString &name, LookupType type)
{
    FUNC2(name, type);
    UserData userData = { name, QStringList(), type, 0 };
    QHash<QString, CXTranslationUnit>::iterator it = m_translationUnits.begin();
    qDebug() << m_translationUnits.keys();
    while (it != m_translationUnits.end()) {
        qDebug() << it.key();
        CXCursor cursor = clang_getTranslationUnitCursor(it.value());
        clang_visitChildren(cursor, lookupSymbol, &userData);
        ++it;
    }
    // qDebug() << "FOOOOO" << noCollisions.elapsed() << userData.count;
    return createResultMap(userData.results.join("\n"));
}

QVariantMap Daemon::loadAST(const QVariantMap &args)
{
    QString filename = args.value(QLatin1String("file")).toString();
    if (filename.isEmpty())
        return createResultMap(QLatin1String("No filename specified (use --file=<filename>)"));
    if (m_translationUnits.contains(filename))
        return createResultMap(QLatin1String("File already loaded"));
    QString prefix = QCoreApplication::applicationDirPath() + QLatin1String("/ast");
    QFileInfo finfo(prefix + filename);
    if (!finfo.exists())
        return createResultMap(QLatin1String("AST file does not exist"));
    CXTranslationUnit unit = clang_createTranslationUnit(m_index, finfo.absoluteFilePath().toLocal8Bit().constData());
    m_translationUnits[filename] = unit;
    return createResultMap(QLatin1String("AST file loaded"));
}

QVariantMap Daemon::saveAST(const QVariantMap &args)
{
    QString filename = args.value(QLatin1String("file")).toString();
    if (filename.isEmpty())
        return createResultMap(QLatin1String("No filename specified (use --file=<filename>)"));
    QHash<QString, CXTranslationUnit>::const_iterator it = m_translationUnits.find(filename);
    if (it == m_translationUnits.end())
        return createResultMap(QLatin1String("No translation unit for filename found"));
    if (writeAST(it))
        return createResultMap(QLatin1String("Saved"));
    return createResultMap(QLatin1String("Unable to save translation unit"));
}

bool Daemon::writeAST(const QHash<QString, CXTranslationUnit>::const_iterator &it)
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

void Daemon::addTranslationUnit(const QString &absoluteFilePath,
                                unsigned options,
                                const QList<QByteArray> &compilerOptions)
{
    FUNC3(absoluteFilePath, options, compilerOptions);
#ifndef USE_THREAD
    const int size = compilerOptions.size();
    QVarLengthArray<const char*, 32> args(size);
    for (int i=0; i<size; ++i) {
        args[i] = compilerOptions.at(i).constData();
    }

    CXTranslationUnit unit = clang_parseTranslationUnit(m_index,
                                                        absoluteFilePath.toLocal8Bit().constData(),
                                                        args.constData(), size, 0, 0,
                                                        options);
    onFileParsed(absoluteFilePath, unit);
#else
    ClangThread *thread = new ClangThread(absoluteFilePath, options, compilerOptions, m_index, this);
    connect(thread, SIGNAL(error(QString)), this, SLOT(onParseError(QString)));
    connect(thread, SIGNAL(fileParsed(QString, void*)),
            this, SLOT(onFileParsed(QString, void*)));
    thread->start();
#endif
}
void Daemon::onParseError(const QString &absoluteFilePath)
{
    FUNC1(absoluteFilePath);
    qWarning("Failed to add %s", qPrintable(absoluteFilePath));
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

static inline Database::Location location(CXCursor cursor)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    unsigned int line, column, offset;
    CXFile file;
    clang_getInstantiationLocation(location, &file, &line, &column, &offset);
    const QByteArray fileName = eatString(clang_getFileName(file));

    const Database::Location loc = {
        fileName.isEmpty() ? QFileInfo() : QFileInfo(QString::fromLocal8Bit(fileName)),
        line,
        column,
        0
    };
    return loc;
}

struct ProcessFileUserData {
    QSet<unsigned> seen;
    int count;
};

static CXChildVisitResult processFile(CXCursor cursor, CXCursor, CXClientData data)
{
    ProcessFileUserData &userData = *reinterpret_cast<ProcessFileUserData*>(data);
    // CXCursor canonical = clang_getCanonicalCursor(cursor);
    // const unsigned hash = clang_hashCursor(canonical);
    // printf(".");
    // static int count = 0;
    // if (++count == 82) {
    //     printf("\n");
    //     count = 0;
    // }

    // if (!userData.seen.contains(hash)) {
    // userData.seen.insert(hash);
    // printf("%s %s %d\n", kindToString(clang_getCursorKind(cursor)),
    //        symbolName(cursor, true).constData(), clang_isCursorDefinition(cursor));
    const CXCursorKind kind = clang_getCursorKind(cursor);
    switch (kind) {
    case CXCursor_ClassDecl:
        if (!clang_isCursorDefinition(cursor)) {// forward declaration
            qDebug() << "dropping forward declaration of" << eatString(clang_getCursorDisplayName(cursor));
            break;
        }
        // fallthrough
    case CXCursor_Namespace:
    case CXCursor_CXXMethod:
    case CXCursor_Constructor: {
        const Database::Location loc = location(cursor);
        if (!loc.file.exists()) {
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                     << kind << "because of missing file" << cursor;
            break;
        }
        const QByteArray symbol = symbolName(cursor);
        int symbolId = Database::symbolId(symbol);
        if (!symbolId) {
            ++userData.count;
            symbolId = Database::addSymbolDeclaration(symbol, loc);
            ++userData.count;
        }
        Q_ASSERT(symbolId);
        ++userData.count;
        Database::addSymbolDefinition(symbolId, loc);
        break; }
    case CXCursor_CallExpr: {
        CXCursor method = clang_getCursorReferenced(cursor);
        if (!isValidCursor(method)) {
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                     << "because of invalid reference" << cursor;
            break;
        }
        const Database::Location callLoc = location(cursor);
        if (!callLoc.file.exists()) {
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                     << "because of missing file" << cursor << "reference" << method;
            break;
        }

        const QByteArray symbol = symbolName(method);
        int symbolId = Database::symbolId(symbol);
        if (!symbolId) {
            const Database::Location loc = location(method);
            if (!loc.file.exists()) { // can't find the file of the method that's being called
                qDebug() << "dropping" << symbol
                         << "because of missing file for reference"
                         << cursor << "method" << method;
                break;
            }
            symbolId = Database::addSymbolDeclaration(symbol, loc);
            ++userData.count;
        }
        Q_ASSERT(symbolId != 0);
        Database::addSymbolReference(symbolId, callLoc);
        ++userData.count;
        break; }
    default:
        break;
    }
    // }

    return CXChildVisit_Recurse;
}

void Daemon::onFileParsed(const QString &absoluteFilePath, void *u)
{
    FUNC2(absoluteFilePath, u);
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    m_fileSystemWatcher.addPath(absoluteFilePath);
    m_translationUnits[absoluteFilePath] = unit;
    // crashes right now with some issue with autoincrement primary key on Symbol
    Database::addFile(absoluteFilePath, QByteArray()); // ### must pass on compiler options
    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    ProcessFileUserData userData;
    userData.count = 0;
    clang_visitChildren(cursor, processFile, &userData);
    qDebug("Added %d entries for %s", userData.count, qPrintable(absoluteFilePath));
}

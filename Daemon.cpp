#include "Daemon.h"
#ifndef EBUS_ENABLED
#include "DaemonAdaptor.h"
#endif
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "ClangJob.h"
#include "Database.h"

static QVariantMap createResultMap(const QString& result)
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
    const QByteArray fileName = eatString(clang_getFileName(file));
    QFileInfo fi(fileName);
    if (fi.exists()) {
        text += QString(", %1:%2:%3").arg(fi.absoluteFilePath()).arg(line).arg(column);
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
        return lookup(args);
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
    CXTranslationUnit unit = m_translationUnits.value(absoluteFilePath);
    if (unit) {
        ClangJob *job = new ClangJob(unit, absoluteFilePath);
        // reparsed signal somehow?
#ifdef USE_THREAD
        m_threadPool.post(job);
#else
        job->execute();
        delete job;
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

static void add(QByteArray &list, char *buf, const Location &location, Type type)
{
    if (location.exists()) {
        const int count = snprintf(buf, 511, "%c \"%s:%d:%d\"\n", type, location.fileName.constData(),
                                   location.line, location.column);
        list += QByteArray::fromRawData(buf, count);
    }
}

QVariantMap Daemon::lookup(const QVariantMap &args)
{
    const QByteArray symbol = args.value(QLatin1String("symbol")).toByteArray();
    if (symbol.isEmpty()) 
        return createResultMap(QLatin1String("No symbol in lookup request"));


    char buffer[512];
    QByteArray results;
    const QStringList symbolTypes = args.value(QLatin1String("types")).toString().
        split(',', QString::SkipEmptyParts);
    // ### uglehack

    qDebug() << symbol << symbolTypes;
    // qDebug() << symbol << args << symbolTypes << Database::symbolDeclarationSize()
    //          << Database::symbolDefinitionSize() << Database::symbolReferencesSize();
    
    if (symbolTypes.isEmpty() || symbolTypes.contains("declaration")) {
        add(results, buffer, Database::lookupDeclaration(symbol), Declaration);
    }
    if (symbolTypes.isEmpty() || symbolTypes.contains("definition")) {
        add(results, buffer, Database::lookupDefinition(symbol), Definition);
    }
    if (symbolTypes.isEmpty() || symbolTypes.contains("reference")) {
        foreach(const Location &loc, Database::lookupReferences(symbol)) {
            add(results, buffer, loc, Reference);
        }
    }
    return createResultMap(results);
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
    ClangJob *job = new ClangJob(absoluteFilePath, options, compilerOptions, m_index);
    connect(job, SIGNAL(error(QString)), this, SLOT(onParseError(QString)));
    connect(job, SIGNAL(fileParsed(QString, void*)),
            this, SLOT(onFileParsed(QString, void*)));
    m_threadPool.post(job);
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

struct ProcessFileUserData {
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
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                     << kindToString(clang_getCursorKind(cursor))
                     << "because of missing file" << cursor << __LINE__;
            break;
        }
        const QByteArray symbol = symbolName(cursor);
        if (symbol.isEmpty()) {
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                     << kindToString(clang_getCursorKind(cursor))
                     << "because of empty symbolName" << __LINE__;
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
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(cursor))
                     << kindToString(clang_getCursorKind(cursor))
                     << "because of we can't find location" << __LINE__;
            break;
        }
        CXCursor method = clang_getCursorReferenced(cursor);
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
            qDebug() << "dropping" << eatString(clang_getCursorDisplayName(method))
                     << kindToString(clang_getCursorKind(method))
                     << "because of empty symbolName" << __LINE__;
            break;
        }
        Location methodLoc = Database::lookupDeclaration(symbol);
        if (!methodLoc.exists()) {
            methodLoc = Location(method);
            if (methodLoc.exists()) {
                Database::setSymbolDeclaration(symbol, methodLoc);
                ++userData.count;
            } else {
                if (Options::s_verbose || symbol != "__va_list_tag()") {
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

void Daemon::onFileParsed(const QString &absoluteFilePath, void *u)
{
    FUNC2(absoluteFilePath, u);
    CXTranslationUnit unit = reinterpret_cast<CXTranslationUnit>(u);
    m_fileSystemWatcher.addPath(absoluteFilePath);
    if (m_translationUnits.contains(absoluteFilePath)) {
        qWarning("We already have this file: %s", qPrintable(absoluteFilePath));
        clang_disposeTranslationUnit(unit);
        return;
    }
    Q_ASSERT(!m_translationUnits.contains(absoluteFilePath));
    m_translationUnits[absoluteFilePath] = unit;
    // crashes right now with some issue with autoincrement primary key on Symbol
    CXCursor cursor = clang_getTranslationUnitCursor(unit);
    ProcessFileUserData userData;
    userData.count = 0;
    QElapsedTimer timer;
    timer.start();
    clang_visitChildren(cursor, processFile, &userData);
    qDebug("Added %d entries for %s (%lld ms)", userData.count, qPrintable(absoluteFilePath), timer.elapsed());
}

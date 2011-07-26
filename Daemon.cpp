#include "Daemon.h"
#ifndef EBUS_ENABLED
#include "DaemonAdaptor.h"
#endif
#include "GccArguments.h"
#include <QCoreApplication>
#include "Utils.h"
#include "ClangThread.h"
#include "Database.h"

static QByteArray eatString(CXString string)
{
    const QByteArray ret = clang_getCString(string);
    clang_disposeString(string);
    return ret;
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

static QString syntax()
{
    FUNC;
    return QLatin1String("Syntax: rtags <command> [argument1, argument2, ...]\n"
                         "commands: syntax|quit|add|remove|lookupline|makefile|daemonize|files|lookup\n");
}

void Daemon::onFileChanged(const QString &path)
{
    FUNC1(path);
    const QFileInfo fi(path);
    if (fi.exists()) {
        addSourceFile(fi);
    } else {
        removeSourceFile(QStringList() << path);
    }
}

QString Daemon::runCommand(const QStringList &a)
{
    FUNC1(a);
    if (a.size() < 2)
        return QLatin1String("No arguments!");

    QStringList args = a;
    QString path = args.first();
    args.removeFirst();
    QString cmd = args.first();
    args.removeFirst();

    if (cmd == QLatin1String("syntax")) {
        return syntax();
    } else if (cmd == QLatin1String("quit")) {
        QTimer::singleShot(100, QCoreApplication::instance(), SLOT(quit()));
        // hack to make the quit command properly respond before the server goes down
        return QLatin1String("quitting");
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
    return QLatin1String("Unknown command");
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

QString Daemon::fileList(const QStringList &args)
{
    FUNC1(args);
    bool seenDashDash = false;
    QString pattern;
    bool regexp = false;
    for (int i=0; i<args.size(); ++i) {
        const QString &arg = args.at(i);
        if (!seenDashDash && arg.startsWith("-")) {
            if (arg == "--") {
                seenDashDash = true;
            } else if (arg == QLatin1String("-r") || arg == QLatin1String("--regexp")) {
                regexp = true;
            } else {
                return QLatin1String("Unknown option ") + arg;
            }// absolute vs relative?
        } else if (pattern.isEmpty()) {
            pattern = arg;
        } else {
            return QLatin1String("Too many args");
        }
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
    return out.join(QLatin1String("\n"));
}


bool Daemon::addSourceFile(const QFileInfo &fi, unsigned options, QString *result)
{
    FUNC2(fi, options);
    if (!fi.exists()) {
        if (result)
            *result = QLatin1String("File doesn't exist");
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
            *result = QLatin1String("Reparsed");
    } else {
        addTranslationUnit(absoluteFilePath, options);
        if (result)
            *result = QLatin1String("Added");
    }
    return true;
}

QString Daemon::addSourceFile(const QStringList &args)
{
    FUNC1(args);
    if (args.isEmpty())
        return QLatin1String("No file to add");
    const QFileInfo finfo(args.first());
    unsigned options = 0;
    for (int i = 1; i < args.size(); ++i) {
        const QString curopt = args.at(i).toLower();
        if (curopt == QLatin1String("incomplete"))
            options |= CXTranslationUnit_Incomplete;
        else if (curopt == QLatin1String("cachecompletion"))
            options |= CXTranslationUnit_CacheCompletionResults;
    }
    QString result;
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
        if (it != m_translationUnits.end())
            m_translationUnits.erase(it);

        // qDebug() << "parsing" << absoluteFilePath << defines << includes;
        addTranslationUnit(absoluteFilePath,
                           CXTranslationUnit_CacheCompletionResults,
                           options);
        // printf("Done %s\n", qPrintable(absoluteFilePath));
    }

    return true;
}

QString Daemon::addMakefile(const QString& path, const QStringList &args)
{
    FUNC2(path, args);
    if (path.isEmpty() || args.isEmpty())
        return QLatin1String("No Makefile to add");

    QString cwd = QDir::currentPath();
    QDir::setCurrent(path);

    QString filename = args.first();
    QFileInfo finfo(filename);
    if (!finfo.exists()) {
        QDir::setCurrent(cwd);
        return QLatin1String("Makefile does not exist") + filename;
    }

    QDir::setCurrent(finfo.absolutePath());

    QProcess proc;
    proc.start(QLatin1String("make"), QStringList() << QLatin1String("-B") << QLatin1String("-n") << QLatin1String("-f") << finfo.fileName());
    if (!proc.waitForFinished(-1)) {
        QDir::setCurrent(cwd);
        return QLatin1String("Unable to wait for make finish");
    }
    if (proc.exitCode() != 0) {
        QDir::setCurrent(cwd);
        return QLatin1String("Make returned error: " + proc.readAllStandardError());
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

    if (!error.isEmpty())
        return error;
    return QString::fromLocal8Bit("Added %1 translation units").arg(m_translationUnits.size());
}

QString Daemon::removeSourceFile(const QStringList &args)
{
    FUNC1(args);
    QString pattern;
    bool regexp = false;
    bool seenDashDash = false;
    foreach(const QString &arg, args) {
        if (!seenDashDash && arg.startsWith(QLatin1Char('-'))) {
            if (arg == QLatin1String("--")) {
                seenDashDash = true;
                continue;
            } else if (arg == QLatin1String("-r") || arg == QLatin1String("--regexp")) {
                regexp = true;
                continue;
            }
        }
        if (!pattern.isEmpty())
            qWarning("Invalid arguments to removeSourceFile");
        pattern = arg;
    }

    // ### need to use regexp and match partial and all that good stuff. Maybe
    // ### make it use the same code path as fileList
    if (pattern.isEmpty())
        return QLatin1String("No file to remove");
    QHash<QString, CXTranslationUnit>::iterator it = m_translationUnits.find(pattern);
    if (it == m_translationUnits.end())
        return QLatin1String("No matches for ") + pattern;
    clang_disposeTranslationUnit(it.value());
    m_fileSystemWatcher.removePath(it.key());
    m_translationUnits.erase(it);
    return QLatin1String("Removed");
}

static bool isValidCursor(CXCursor cursor)
{
    FUNC;
    CXCursorKind kind = clang_getCursorKind(cursor);
    return !clang_isInvalid(kind);
}

QString Daemon::lookupLine(const QStringList &args)
{
    FUNC1(args);
    if (args.size() != 3)
        return QLatin1String("Invalid argument count");

    bool ok;

    QString filename = args.at(0);
    int line = args.at(1).toInt(&ok);
    if (!ok)
        return QLatin1String("Argument 2 is not an int");
    int column = args.at(2).toInt(&ok);
    if (!ok)
        return QLatin1String("Argument 3 is not an int");

    if (!m_translationUnits.contains(filename))
        return QLatin1String("Translation unit not found");

    CXTranslationUnit unit = m_translationUnits.value(filename);
    CXFile file = clang_getFile(unit, filename.toLocal8Bit().constData());

    CXSourceLocation location = clang_getLocation(unit, file, line, column);
    CXCursor cursor = clang_getCursor(unit, location);
    if (!isValidCursor(cursor))
        return QLatin1String("Unable to get cursor for location");

    CXCursorKind kind = clang_getCursorKind(cursor);
    CXCursor referenced;
    if (kind == CXCursor_CXXMethod) // might need to add more here
        referenced = clang_getCanonicalCursor(cursor);
    else
        referenced = clang_getCursorReferenced(cursor);
    if (!isValidCursor(referenced))
        return QLatin1String("No referenced cursor");

    location = clang_getCursorLocation(referenced);
    unsigned int rline, rcolumn, roffset;
    CXFile rfile;
    clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
    CXString rfilename = clang_getFileName(rfile);

    QString ret = QString("Symbol (decl) at %1, line %2 column %3").
        arg(clang_getCString(rfilename)).
        arg(rline).arg(rcolumn);

    clang_disposeString(rfilename);

    return ret;
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
        ret += cursorData(clang_getCursorReferenced(cursor));
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

QString Daemon::lookup(const QString &name, LookupType type)
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
    qDebug() << "FOOOOO" << noCollisions.elapsed() << userData.count;
    return userData.results.join("\n");
}

QString Daemon::loadAST(const QStringList &args)
{
    if (args.isEmpty())
        return QLatin1String("No filename specified");
    QString filename = args.first();
    if (m_translationUnits.contains(filename))
        return QLatin1String("File already loaded");
    QString prefix = QCoreApplication::applicationDirPath() + QLatin1String("/ast");
    QFileInfo finfo(prefix + filename);
    if (!finfo.exists())
        return QLatin1String("AST file does not exist");
    CXTranslationUnit unit = clang_createTranslationUnit(m_index, finfo.absoluteFilePath().toLocal8Bit().constData());
    m_translationUnits[filename] = unit;
    return QLatin1String("AST file loaded");
}

QString Daemon::saveAST(const QStringList &args)
{
    if (args.isEmpty())
        return QLatin1String("No filename specified");
    QString filename = args.first();
    QHash<QString, CXTranslationUnit>::const_iterator it = m_translationUnits.find(filename);
    if (it == m_translationUnits.end())
        return QLatin1String("No translation unit for filename found");
    if (writeAST(it))
        return QLatin1String("Saved");
    return QLatin1String("Unable to save translation unit");
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

static inline QByteArray symbolName(CXCursor cursor, bool noWarning = false)
{
    QByteArray name;
    bool done = false;
    forever {
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_CXXMethod:
        case CXCursor_Constructor:
            Q_ASSERT(name.isEmpty());
            name = eatString(clang_getCursorDisplayName(cursor));
            break;
        case CXCursor_ClassDecl:
        case CXCursor_Namespace:
            if (!name.isEmpty())
                name.prepend("::");
            name.prepend(eatString(clang_getCursorDisplayName(cursor)));
            break;
        case CXCursor_TranslationUnit:
        case CXCursor_FirstInvalid:
            done = true;
            break;
        case CXCursor_UnexposedDecl:
            break;
        default:
            if (!noWarning)
                qWarning("Got unexpected parent %s %s",
                         kindToString(clang_getCursorKind(cursor)),
                         eatString(clang_getCursorDisplayName(cursor)).constData());
            if (name.isEmpty())
                name = eatString(clang_getCursorDisplayName(cursor));
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
    QByteArray fileName = eatString(clang_getFileName(file));

    const Database::Location loc = {
        QFileInfo(QString::fromLocal8Bit(fileName)),
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
    CXCursor canonical = clang_getCanonicalCursor(cursor);
    const unsigned hash = clang_hashCursor(canonical);
    // printf(".");
    // static int count = 0;
    // if (++count == 82) {
    //     printf("\n");
    //     count = 0;
    // }

    if (!userData.seen.contains(hash)) {
        // userData.seen.insert(hash);
        printf("%s %s %d\n", kindToString(clang_getCursorKind(cursor)),
               symbolName(cursor, true).constData(), clang_isCursorDefinition(cursor));
        const CXCursorKind kind = clang_getCursorKind(cursor);
        switch (kind) {
        case CXCursor_ClassDecl:
            if (!clang_isCursorDefinition(cursor)) // forward declaration
                break;
        case CXCursor_CXXMethod:
        case CXCursor_Constructor: {
            const QByteArray symbol = symbolName(cursor);
            int symbolId = Database::symbolId(symbol);
            Database::Location loc = { QFileInfo(), -1, -1, -1 };
            if (!symbolId) {
                loc = location(cursor);
                symbolId = Database::addSymbol(symbol, location(cursor));
                ++userData.count;
            }
            Q_ASSERT(symbolId);
            if (kind != CXCursor_ClassDecl && clang_isCursorDefinition(cursor)) {
                if (loc.line == -1)
                    loc = location(cursor);
                Database::addSymbolDefinition(symbolId, loc);
            }
            break; }
        case CXCursor_CallExpr: {
            CXCursor method = clang_getCursorReferenced(cursor);
            QByteArray symbol = symbolName(method);
            int symbolId = Database::symbolId(symbol);
            if (!symbolId) {
                symbolId = Database::addSymbol(symbol, location(method));
                ++userData.count;
            }
            Q_ASSERT(symbolId != 0);
            Database::addSymbolReference(symbolId, location(cursor));
            ++userData.count;
            break; }
        default:
            break;
        }
    }
    
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

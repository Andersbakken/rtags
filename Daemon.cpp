#include "Daemon.h"
#include "DaemonAdaptor.h"
#include "GccArguments.h"
#include <QCoreApplication>
#include "Daemon_p.h"
#ifdef EBUS
#include "Ebus.h"
#endif

Daemon::Daemon(QObject *parent)
    : QObject(parent), m_index(clang_createIndex(1, 0))
#ifdef EBUS
    , m_server(0)
#endif
{
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
#ifndef EBUS
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
    if (!m_server->listen(QHostAddress::LocalHost, ::port())) {
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
        clang_reparseTranslationUnit(unit, 0, 0, 0);
        if (result)
            *result = QLatin1String("Reparsed");
    } else {
        unit = clang_parseTranslationUnit(m_index, absoluteFilePath.toLocal8Bit().constData(),
                                          0, 0, 0, 0, options);
        m_fileSystemWatcher.addPath(absoluteFilePath);
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

    if (!args.isCompile()) // Just accept link lines without doing anything
        return true;

    QList<QByteArray> includes = args.arguments("-I");
    QList<QByteArray> defines = args.arguments("-D");

    const int parseargssize = includes.size() + defines.size();
    char const ** parseargs = new char const *[parseargssize];
    int pos = 0;
    foreach(const QByteArray& inc, includes) {
        parseargs[pos++] = inc.constData();
    }
    foreach(const QByteArray& def, defines) {
        parseargs[pos++] = def.constData();
    }

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
        {
            // Timer t(__FUNCTION__, __LINE__);
            CXTranslationUnit unit = clang_parseTranslationUnit(m_index,
                                                                absoluteFilePath.toLocal8Bit().constData(),
                                                                parseargs, parseargssize, 0, 0,
                                                                CXTranslationUnit_CacheCompletionResults);
            m_translationUnits[absoluteFilePath] = unit;
            m_fileSystemWatcher.addPath(absoluteFilePath);
        }
    }
    // printf("Done\n");

    delete[] parseargs;
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
    return QLatin1String("Added");
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
};

static enum CXChildVisitResult lookupSymbol(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
    FUNC;
    UserData *data = reinterpret_cast<UserData*>(client_data);
    CXString usr = clang_getCursorDisplayName(cursor);
    CXString usr2 = clang_getCursorSpelling(cursor);
    CXSourceLocation location = clang_getCursorLocation(cursor);
    unsigned int rline, rcolumn, roffset;
    CXFile rfile;
    clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
    CXString rfilename = clang_getFileName(rfile);
    QString ret = QString("Symbol (decl) at %1, line %2 column %3").
        arg(clang_getCString(rfilename)).
        arg(rline).arg(rcolumn);

    printf("%s => %s => %s\n", clang_getCString(usr), clang_getCString(usr2),
           qPrintable(ret));
    clang_disposeString(usr);
    clang_disposeString(usr2);
    clang_disposeString(rfilename);
    

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
    UserData userData = { name, QStringList(), type };
    QHash<QString, CXTranslationUnit>::iterator it = m_translationUnits.begin();
    qDebug() << m_translationUnits.keys();
    while (it != m_translationUnits.end()) {
        qDebug() << it.key();
        CXCursor cursor = clang_getTranslationUnitCursor(it.value());
        clang_visitChildren(cursor, lookupSymbol, &userData);
        ++it;
    }
    return userData.results.join("\n");
}

bool Daemon::writeAST(const QHash<QString, CXTranslationUnit>::const_iterator &it)
{
    FUNC;
}

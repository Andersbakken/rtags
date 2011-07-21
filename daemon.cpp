#include "daemon.h"
#include "daemonadaptor.h"
#include "gccargs.h"
#include <QCoreApplication>
#ifdef EBUS
#include "utils.h"
#endif

Daemon::Daemon(QObject *parent)
    : QObject(parent), m_index(clang_createIndex(1, 0))
#ifdef EBUS
    , m_server(0)
#endif
{
    connect(&m_fileSystemWatcher, SIGNAL(fileChanged(QString)), this, SLOT(onFileChanged(QString)));
}

Daemon::~Daemon()
{
    foreach(CXTranslationUnit unit, m_translationUnits) {
        clang_disposeTranslationUnit(unit);
    }
    clang_disposeIndex(m_index);
}

bool Daemon::start()
{
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
    return QLatin1String("Syntax: rtags <command> [argument1, argument2, ...]\n"
                         "commands: syntax|quit|add|remove|lookupline|makefile|daemonize\n");
}

void Daemon::onFileChanged(const QString &path)
{
    const QFileInfo fi(path);
    if (fi.exists()) {
        addSourceFile(fi);
    } else {
        removeSourceFile(path);
    }
}

QString Daemon::runCommand(const QStringList &a)
{
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
        return removeSourceFile(args.value(0));
    } else if (cmd == QLatin1String("lookupline")) {
        return lookupLine(args);
    } else if (cmd == QLatin1String("makefile")) {
        return addMakefile(path, args);
    }
    return QLatin1String("Unknown command");
}

bool Daemon::addSourceFile(const QFileInfo &fi, unsigned options, QString *result)
{
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
    // ### check if gcc/g++ really is the compiler used here
    GccArguments args;
    if (!args.parse(line) || !args.hasInput())
        return false;

    const QFileInfo fi(QString::fromLocal8Bit(args.input().constData()));
    if (!fi.exists())
        return false;
    const QString absoluteFilePath = fi.absoluteFilePath();
    if (m_translationUnits.contains(absoluteFilePath)) {
        removeSourceFile(absoluteFilePath);
    }

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

    CXTranslationUnit unit = clang_parseTranslationUnit(m_index,
                                                        absoluteFilePath.toLocal8Bit().constData(),
                                                        parseargs, parseargssize, 0, 0,
                                                        CXTranslationUnit_CacheCompletionResults);
    m_translationUnits[absoluteFilePath] = unit;
    m_fileSystemWatcher.addPath(absoluteFilePath);

    delete[] parseargs;
    return true;
}

QString Daemon::addMakefile(const QString& path, const QStringList &args)
{
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
        if (!addMakefileLine(lineOpts))
            error += QLatin1String("Unable to add ") + makeLine + QLatin1String("\n");
    }

    QDir::setCurrent(cwd);

    if (!error.isEmpty())
        return error;
    return QLatin1String("Added");
}

QString Daemon::removeSourceFile(const QString &file)
{
    if (file.isEmpty())
        return QLatin1String("No file to remove");
    QHash<QString, CXTranslationUnit>::iterator it = m_translationUnits.find(file);
    if (it == m_translationUnits.end())
        return QLatin1String("File is not parsed");
    clang_disposeTranslationUnit(it.value());
    m_translationUnits.erase(it);
    m_fileSystemWatcher.removePath(file);
    return QLatin1String("Removed");
}

static bool isValidCursor(CXCursor cursor)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    return !clang_isInvalid(kind);
}

struct FindParentVisitorData
{
    CXString find;
    bool found;
    CXCursor result;
};

static enum CXChildVisitResult findParentVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
    Q_UNUSED(parent);

    FindParentVisitorData* data = reinterpret_cast<FindParentVisitorData*>(client_data);
    CXString usr = clang_getCursorUSR(cursor);
    if (!strcmp(clang_getCString(data->find), clang_getCString(usr))) {
        data->result = cursor;
        data->found = true;
        clang_disposeString(usr);
        return CXChildVisit_Break;
    }
    clang_disposeString(usr);
    return CXChildVisit_Continue;
}

static CXCursor findParentUSRDecl(CXCursor parent, CXString usr)
{
    FindParentVisitorData data;
    data.find = usr;
    data.found = false;
    clang_visitChildren(parent, findParentVisitor, &data);
    if (data.found)
        return data.result;
    return clang_getNullCursor();
}

QString Daemon::lookupLine(const QStringList &args)
{
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

    cursor = clang_getCanonicalCursor(cursor);

    CXCursor referenced = clang_getCursorReferenced(cursor);
    if (!isValidCursor(referenced))
        return QLatin1String("No referenced cursor");

    if (clang_equalCursors(cursor, referenced)) {
        // hmm
        CXString usr = clang_getCursorUSR(referenced);
        CXCursor parent = clang_getCursorSemanticParent(referenced);
        referenced = findParentUSRDecl(parent, usr);
        clang_disposeString(usr);
        if (!isValidCursor(referenced))
            return QLatin1String("No referenced cursor");
    }

    location = clang_getCursorLocation(referenced);
    unsigned int rline, rcolumn, roffset;
    CXFile rfile;
    clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
    CXString rfilename = clang_getFileName(rfile);

    QString ret = QString("Symbol (decl) at %1, line %2 column %3").arg(clang_getCString(rfilename)).arg(rline).arg(rcolumn);

    clang_disposeString(rfilename);

    return ret;
}

#ifdef EBUS
void Daemon::onNewConnection()
{
    Q_ASSERT(m_server->hasPendingConnections());
    QTcpSocket *sock = m_server->nextPendingConnection();
    Q_ASSERT(sock);
    m_connections[sock] = -1;
    connect(sock, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    connect(sock, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    read(sock);
}

void Daemon::onReadyRead()
{
    read(qobject_cast<QTcpSocket*>(sender()));
}

void Daemon::onDisconnected()
{
    QTcpSocket *sock = qobject_cast<QTcpSocket*>(sender());
    disconnect(sock, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    disconnect(sock, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    m_connections.remove(sock);
    sock->deleteLater();
}

void Daemon::read(QTcpSocket *socket)
{
    Q_ASSERT(socket);
    Q_ASSERT(m_connections.contains(socket));
    QStringList arguments;
    qint16 &size = m_connections[socket];
    switch (::readFromSocket(socket, arguments, size)) {
    case Error:
        qWarning("Couldn't send message to daemon");
        socket->disconnect();
        break;
    case WaitForData:
        break;
    case Finished:
        ::writeToSocket(socket, runCommand(arguments));
        break;
    }
}
#endif

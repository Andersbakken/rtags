#include "daemon.h"
#include "daemonadaptor.h"
#include "gccargs.h"
#include <QCoreApplication>

Daemon::Daemon(QObject *parent)
    : QObject(parent), m_index(clang_createIndex(1, 0))
{
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
    DaemonAdaptor* adaptor = new DaemonAdaptor(this);

    QDBusConnection dbus = QDBusConnection::sessionBus();
    if (!dbus.registerObject(QLatin1String("/"), this)) {
        printf("%s %d: if (!dbus.registerObject(QLatin1String(\"/\"), this)) {\n", __FILE__, __LINE__);
        delete adaptor;
        return false;
    }
    if (!dbus.registerService(QLatin1String("rtags.Daemon"))) {
        printf("%s %d: if (!dbus.registerService(QLatin1String(\"rtags.Daemon\"))) {\n", __FILE__, __LINE__);
        delete adaptor;
        return false;
    }

    return true;
}

static QString syntax()
{
    return QLatin1String("Syntax: rtags <command> [argument1, argument2, ...]");
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

    if (cmd == QLatin1String("syntax"))
        return syntax();
    else if (cmd == QLatin1String("quit"))
        QCoreApplication::quit();
    else if (cmd == QLatin1String("add"))
        return addSourceFile(args);
    else if (cmd == QLatin1String("remove"))
        return removeSourceFile(args);
    else if (cmd == QLatin1String("lookupline"))
        return lookupLine(args);
    else if (cmd == QLatin1String("makefile"))
        return addMakefile(path, args);
    else
        return QLatin1String("Unknown command");
    return QString();
}

QString Daemon::addSourceFile(const QStringList &args)
{
    if (args.isEmpty())
        return QLatin1String("No file to add");
    QString filename = args.first();
    QFileInfo finfo(filename);
    if (!finfo.exists())
        return QLatin1String("File does not exist");

    if (m_translationUnits.contains(filename)) {
        CXTranslationUnit unit = m_translationUnits.value(filename);
        clang_reparseTranslationUnit(unit, 0, 0, 0);
        return QLatin1String("Reparsed");
    } else {
        unsigned options = 0;
        for (int i = 1; i < args.size(); ++i) {
            const QString curopt = args.at(i).toLower();
            if (curopt == QLatin1String("incomplete"))
                options |= CXTranslationUnit_Incomplete;
            else if (curopt == QLatin1String("cachecompletion"))
                options |= CXTranslationUnit_CacheCompletionResults;
        }

        CXTranslationUnit unit = clang_parseTranslationUnit(m_index, filename.toLocal8Bit().constData(),
                                                            0, 0, 0, 0, options);
        m_translationUnits[filename] = unit;
        return QLatin1String("Added");
    }

    return QString();
}

bool Daemon::addMakefileLine(const QList<QByteArray> &line)
{
    // ### check if gcc/g++ really is the compiler used here
    GccArguments args;
    args.parse(line);
    if (!args.hasInput())
        return false;

    QString filename = QString::fromLocal8Bit(args.input().constData());
    if (m_translationUnits.contains(filename))
        return false;

    CXTranslationUnit unit = clang_parseTranslationUnit(m_index, args.input().constData(), 0, 0, 0, 0,
                                                        CXTranslationUnit_CacheCompletionResults);
    m_translationUnits[filename] = unit;
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
    if (proc.exitCode() != 0 || !proc.readAllStandardError().isEmpty()) {
        QDir::setCurrent(cwd);
        return QLatin1String("Make returned error");
    }

    QString error;
    QList<QByteArray> makeData = proc.readAllStandardOutput().split('\n');
    foreach(const QByteArray& makeLine, makeData) {
        if (makeLine.isEmpty())
            continue;
        // ### this should be improved with quote support
        QList<QByteArray> lineOpts = makeLine.split(' ');
        if (!addMakefileLine(lineOpts))
            error += QLatin1String("Unable to add") + makeLine + QLatin1String("\n");
    }

    QDir::setCurrent(cwd);

    if (!error.isEmpty())
        return error;
    return QLatin1String("Added");
}

QString Daemon::removeSourceFile(const QStringList &args)
{
    if (args.isEmpty())
        return QLatin1String("No file to remove");

    QHash<QString, CXTranslationUnit>::iterator it = m_translationUnits.find(args.first());
    if (it == m_translationUnits.end())
        return QLatin1String("File is not parsed");
    clang_disposeTranslationUnit(it.value());
    m_translationUnits.erase(it);

    return QLatin1String("Removed");
}

static bool isValidCursor(CXCursor cursor)
{
    CXCursorKind kind = clang_getCursorKind(cursor);
    return !clang_isInvalid(kind);
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

    if (clang_isCursorDefinition(cursor)) {
        CXType type = clang_getCursorType(cursor);
        if (type.kind == CXType_Invalid)
            return QLatin1String("Invalid type for definition cursor");
        CXCursor declaration = clang_getTypeDeclaration(type);
        if (!isValidCursor(declaration))
            return QLatin1String("Unable to get cursor for type declaration");

        location = clang_getCursorLocation(declaration);

        unsigned int rline, rcolumn, roffset;
        CXFile rfile;
        clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
        CXString rfilename = clang_getFileName(rfile);

        return QString("Symbol (decl) at %1, line %2 column %3").arg(clang_getCString(rfilename)).arg(rline).arg(rcolumn);
    } else {
        CXCursor definition = clang_getCursorDefinition(cursor);
        if (!isValidCursor(definition))
            return QLatin1String("Unable to get cursor for definition");

        location = clang_getCursorLocation(definition);

        unsigned int rline, rcolumn, roffset;
        CXFile rfile;
        clang_getInstantiationLocation(location, &rfile, &rline, &rcolumn, &roffset);
        CXString rfilename = clang_getFileName(rfile);

        return QString("Symbol (def) at %1, line %2 column %3").arg(clang_getCString(rfilename)).arg(rline).arg(rcolumn);
    }

    return QLatin1String("Symbol not found");
}

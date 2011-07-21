#include "daemon.h"
#include "daemonadaptor.h"
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
        delete adaptor;
        return false;
    }
    if (!dbus.registerService(QLatin1String("rtags.Daemon"))) {
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
    if (a.isEmpty())
        return QLatin1String("No arguments!");

    QStringList args = a;
    QString arg0 = args.first();
    args.removeFirst();

    if (arg0 == QLatin1String("syntax"))
        return syntax();
    else if (arg0 == QLatin1String("quit"))
        QCoreApplication::quit();
    else if (arg0 == QLatin1String("add"))
        return addSourceFile(args);
    else if (arg0 == QLatin1String("remove"))
        return removeSourceFile(args);
    else if (arg0 == QLatin1String("lookupline"))
        return lookupLine(args);
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
        unsigned options = CXTranslationUnit_CacheCompletionResults;
        for (int i = 1; i < args.size(); ++i) {
            if (args.at(i).toLower() == QLatin1String("incomplete"))
                options |= CXTranslationUnit_Incomplete;
        }

        CXTranslationUnit unit = clang_parseTranslationUnit(m_index, filename.toLocal8Bit().constData(),
                                                            0, 0, 0, 0, options);
        m_translationUnits[filename] = unit;
        return QLatin1String("Added");
    }

    return QString();
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
        return QLatin1String("Document not found");

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

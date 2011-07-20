#include "daemon.h"
#include "daemonadaptor.h"
#include <CppDocument.h>
#include <TypeOfExpression.h>
#include <Symbol.h>
#include <Name.h>
#include <Literals.h>
#include <QCoreApplication>

Daemon::Daemon(QObject *parent)
    : QObject(parent), m_snapshot(new CPlusPlus::Snapshot())
{
}

Daemon::~Daemon()
{
    delete m_snapshot;
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
    QFile file(args.first());
    if (!file.open(QFile::ReadOnly))
        return QLatin1String("File not found");

    CPlusPlus::Document::Ptr doc = CPlusPlus::Document::create(file.fileName());
    doc->setSource(file.readAll());
    doc->check();
    m_snapshot->insert(doc);

    return QLatin1String("Added");
}

static CPlusPlus::Symbol* lookupSymbolString(CPlusPlus::Document::Ptr& doc, CPlusPlus::Snapshot& snapshot,
                                             const QByteArray& linetext, int line, int column)
{
    CPlusPlus::Scope* scope = doc->scopeAt(line, column);
    if (!scope)
        return 0;

    QString exp = QString::fromLocal8Bit(linetext.constData());

    CPlusPlus::TypeOfExpression typeOfExpression;
    typeOfExpression.init(doc, snapshot);
    QList<CPlusPlus::LookupItem> items = typeOfExpression.reference(exp, scope, CPlusPlus::TypeOfExpression::Preprocess);
    if (items.isEmpty())
        return 0;
    return items.first().declaration();
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

    if (!m_snapshot->contains(filename))
        return QLatin1String("Document not found");

    CPlusPlus::Document::Ptr doc = m_snapshot->document(filename);

    QByteArray src = doc->source();
    QList<QByteArray> lines = src.split('\n');

    if (line < 1 || line > lines.size())
        return QLatin1String("Line outside of document bounds");

    qDebug() << "Looking up symbol at" << filename << line << column << lines.at(line - 1);
    // ### this function might need more context (more lines) to determine the symbol
    CPlusPlus::Symbol* symbol = lookupSymbolString(doc, *m_snapshot, lines.at(line - 1), line - 1, column - 1);
    if (!symbol)
        return QLatin1String("Symbol not found");

    return QString("Symbol found at %1:%2.%3").arg(symbol->fileName()).arg(symbol->line() + 1).arg(symbol->column() + 1);
}

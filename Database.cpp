#include "Database.h"
#include <QTextStream>
#include <QStringList>
#include <QVariant>
#include <QDateTime>

static QHash<QByteArray, Location> s_definitions, s_declarations, s_references;
static inline bool isValid(const Location &location)
{
    return (location.exists() && location.line > 0 && location.column > 0);
}

void Database::setSymbolDefinition(const QByteArray &symbolName, const Location &location)
{
    Q_ASSERT(isValid(location));
    s_definitions[symbolName] = location;
}

void Database::setSymbolDeclaration(const QByteArray &symbolName, const Location &location)
{
    Q_ASSERT(isValid(location));
    s_declarations[symbolName] = location;
}

void Database::addSymbolReference(const QByteArray &symbolName, const Location &location)
{
    Q_ASSERT(isValid(location));
    s_references.insertMulti(symbolName, location);
}

Location Database::lookupDeclaration(const QByteArray &symbolName)
{
    return s_declarations.value(symbolName);
}

Location Database::lookupDefinition(const QByteArray &symbolName)
{
    return s_definitions.value(symbolName);
}

QList<Location> Database::lookupReferences(const QByteArray &symbolName)
{
    return s_references.values(symbolName);
}

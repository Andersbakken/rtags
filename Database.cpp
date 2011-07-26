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

void Database::clear()
{
    s_definitions.clear();
    s_declarations.clear();
    s_references.clear();
}

void Database::setSymbolDeclaration(const QByteArray &symbolName, const Location &location)
{
    Q_ASSERT(isValid(location));
    s_declarations[symbolName] = location;
}

bool Database::clearSymbolDeclaration(const QByteArray &symbolName)
{
    return s_declarations.remove(symbolName);
}

Location Database::lookupDeclaration(const QByteArray &symbolName)
{
    return s_declarations.value(symbolName);
}

int Database::symbolDeclarationSize()
{
    return s_declarations.size();
}



void Database::setSymbolDefinition(const QByteArray &symbolName, const Location &location)
{
    Q_ASSERT(isValid(location));
    s_definitions[symbolName] = location;
}

bool Database::clearSymbolDefinition(const QByteArray &symbolName)
{
    return s_definitions.remove(symbolName);
}

Location Database::lookupDefinition(const QByteArray &symbolName)
{
    return s_definitions.value(symbolName);
}

int Database::symbolDefinitionSize()
{
    return s_definitions.size();
}

void Database::addSymbolReference(const QByteArray &symbolName, const Location &location)
{
    Q_ASSERT(isValid(location));
    s_references.insertMulti(symbolName, location);
}

QList<Location> Database::lookupReferences(const QByteArray &symbolName)
{
    return s_references.values(symbolName);
}

int Database::clearSymbolReferences(const QByteArray &symbolName)
{
    return s_references.remove(symbolName);
}

int Database::symbolReferencesSize()
{
    return s_references.size();
}

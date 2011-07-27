#include "Database.h"
#include <QTextStream>
#include <QStringList>
#include <QVariant>
#include <QDateTime>

static QHash<QByteArray, Location> s_definitions, s_declarations;
static QHash<QByteArray, QList<Location> > s_references;

static inline bool isValid(const Location &location)
{
    return (location.exists() && location.line > 0 && location.column > 0);
}

// template <typename T>
// static T find(const QHash<QByteArray, T> &hash, const QByteArray &name)
// {
//     typedef QHash<QByteArray, T> Hash;
//     Hash::iterator it;
//     QHash<QByteArray, T>:
//     for (QHash<QByteArray, T>::const_iterator it = hash.begin(); it != hash.end(); ++it) {
//         if (it.key().contains(name))
//             return it.value();
//     }
//     return T();
// }

void Database::clear()
{
    s_definitions.clear();
    s_declarations.clear();
    s_references.clear();
}

void Database::setSymbolDeclaration(const QByteArray &symbolName, const Location &location)
{
    // qDebug() << "setSymbolDeclaration" << symbolName << location;
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
    // qDebug() << "setSymbolDefinition" << symbolName << location;
    Q_ASSERT(isValid(location));
    s_definitions[symbolName] = location;
}

bool Database::clearSymbolDefinition(const QByteArray &symbolName)
{
    return s_definitions.remove(symbolName);
}

Location Database::lookupDefinition(const QByteArray &symbolName)
{
    // qDebug() << symbolName << s_definitions.keys() << s_definitions.contains(symbolName);
    return s_definitions.value(symbolName);
}

int Database::symbolDefinitionSize()
{
    return s_definitions.size();
}

void Database::addSymbolReference(const QByteArray &symbolName, const Location &location)
{
    // qDebug() << "addSymbolReference" << symbolName << location;
    Q_ASSERT(isValid(location));
    s_references[symbolName] += location;
}

QList<Location> Database::lookupReferences(const QByteArray &symbolName)
{
    QList<Location> ret;
    for (QHash<QByteArray, QList<Location> >::const_iterator it = s_references.begin(); it != s_references.end(); ++it) {
        if (it.key().contains(symbolName))
            ret += it.value();
    }

    return ret;
}

int Database::clearSymbolReferences(const QByteArray &symbolName)
{
    return s_references.remove(symbolName);
}

int Database::symbolReferencesSize()
{
    return s_references.size();
}

#include "Database.h"
#include <QTextStream>
#include <QStringList>
#include <QVariant>
#include <QDateTime>
#include <QReadWriteLock>

static QReadWriteLock s_definitionsLock, s_declarationsLock, s_referencesLock, s_filesLock;
static QList<Symbol> s_definitions, s_declarations, s_references;
static QHash<QByteArray, QList<QByteArray> > s_files;

static inline bool isValid(const Location &location)
{
    return (location.exists() && location.line > 0 && location.column > 0);
}

static inline int findExact(const QByteArray &symbolName, const QList<Symbol> &list, int from = 0)
{
    const int size = list.size();
    for (int i=from; i<size; ++i) {
        if (list.at(i).symbolName == symbolName)
            return i;
    }
    return -1;
}

static inline int findContains(const QByteArray &symbolName, const QList<Symbol> &list, int from = 0)
{
    const int size = list.size();
    for (int i=from; i<size; ++i) {
        if (list.at(i).symbolName.contains(symbolName))
            return i;
    }
    return -1;
}

typedef int (*FindFunc)(const QByteArray &, const QList<Symbol> &, int);

void Database::clear()
{
    {
        QWriteLocker lock(&s_definitionsLock);
        s_definitions.clear();
    }
    {
        QWriteLocker lock(&s_declarationsLock);
        s_declarations.clear();
    }
    {
        QWriteLocker lock(&s_referencesLock);
        s_references.clear();
    }
    {
        QWriteLocker lock(&s_filesLock);
        s_files.clear();
    }
}

void Database::addFile(const QByteArray &file, const QList<QByteArray> &compilerOptions)
{
    QWriteLocker lock(&s_filesLock);
    s_files[file] = compilerOptions;
}

bool Database::removeFile(const QByteArray &file)
{
    QWriteLocker lock(&s_filesLock);
    return s_files.remove(file);
}

QList<QByteArray> Database::compilerOptions(const QByteArray &absoluteFilePath)
{
    QReadLocker lock(&s_filesLock);
    return s_files.value(absoluteFilePath);
}

QList<QByteArray> Database::takeCompilerOptions(const QByteArray &absoluteFilePath)
{
    QWriteLocker lock(&s_filesLock);
    return s_files.take(absoluteFilePath);
}

void Database::setSymbolDeclaration(const QByteArray &symbolName, const Location &location)
{
    QWriteLocker lock(&s_declarationsLock);
    qDebug() << "setSymbolDeclaration" << symbolName << location;
    Q_ASSERT(isValid(location));
    const int idx = findExact(symbolName, s_declarations);
    if (idx != -1) {
        s_declarations[idx].location = location;
    } else {
        s_declarations.append(Symbol(symbolName, location));
    }
}

bool Database::clearSymbolDeclaration(const QByteArray &symbolName)
{
    QWriteLocker lock(&s_declarationsLock);
    // ### is it better to use a linkedlist
    const int idx = findExact(symbolName, s_declarations);
    if (idx != -1) {
        s_declarations.removeAt(idx);
        return true;
    }
    return false;
}

QList<Symbol> Database::lookupDeclarations(const QByteArray &symbolName, bool exactMatch)
{
    QReadLocker lock(&s_declarationsLock);
    QList<Symbol> symbols;
    FindFunc func = exactMatch ? findExact : findContains;
    int idx = 0;
    while ((idx = func(symbolName, s_declarations, idx)) != -1) {
        symbols.append(s_declarations.at(idx++));
    }
    return symbols;
}

int Database::symbolDeclarationSize()
{
    QReadLocker lock(&s_declarationsLock);
    return s_declarations.size();
}

void Database::setSymbolDefinition(const QByteArray &symbolName, const Location &location)
{
    qDebug() << "setSymbolDefinition" << symbolName << location;
    QWriteLocker lock(&s_definitionsLock);
    Q_ASSERT(isValid(location));
    const int idx = findExact(symbolName, s_definitions);
    if (idx != -1) {
        s_definitions[idx].location = location;
    } else {
        s_definitions.append(Symbol(symbolName, location));
    }
}

bool Database::clearSymbolDefinition(const QByteArray &symbolName)
{
    QWriteLocker lock(&s_definitionsLock);
    // ### is it better to use a linkedlist
    const int idx = findExact(symbolName, s_definitions);
    if (idx != -1) {
        s_definitions.removeAt(idx);
        return true;
    }
    return false;
}

QList<Symbol> Database::lookupDefinitions(const QByteArray &symbolName, bool exactMatch)
{
    QReadLocker lock(&s_definitionsLock);
    QList<Symbol> symbols;
    FindFunc func = exactMatch ? findExact : findContains;
    int idx = 0;
    while ((idx = func(symbolName, s_definitions, idx)) != -1) {
        symbols.append(s_definitions.at(idx++));
    }
    return symbols;
}

int Database::symbolDefinitionSize()
{
    QReadLocker lock(&s_definitionsLock);
    return s_definitions.size();
}

void Database::addSymbolReference(const QByteArray &symbolName, const Location &location)
{
    QWriteLocker lock(&s_referencesLock);
    qDebug() << "addSymbolReference" << symbolName << location;
    Q_ASSERT(isValid(location));
    s_references.append(Symbol(symbolName, location));
}

QList<Symbol> Database::lookupReferences(const QByteArray &symbolName, bool exactMatch)
{
    QReadLocker lock(&s_referencesLock);
    QList<Symbol> symbols;
    FindFunc func = exactMatch ? findExact : findContains;
    int idx = 0;
    while ((idx = func(symbolName, s_references, idx)) != -1) {
        symbols.append(s_references.at(idx++));
    }
    return symbols;
}

int Database::clearSymbolReferences(const QByteArray &symbolName)
{
    QWriteLocker lock(&s_referencesLock);
    int idx = 0;
    int count = 0;
    while ((idx = findExact(symbolName, s_references, idx)) != -1) {
        s_references.removeAt(idx);
        ++count;
    }
    return count;
}

int Database::symbolReferencesSize()
{
    QReadLocker lock(&s_referencesLock);
    return s_references.size();
}

void Database::removeReferences(const QByteArray &absoluteFilePath)
{
    QWriteLocker lock(&s_referencesLock);
    // ### leave compiler options stored for this file in database
    QList<Symbol> *lists[] = { &s_definitions, &s_declarations, &s_references };
    for (int i=0; i<3; ++i) {
        QList<Symbol> &list = *lists[i];
        QList<Symbol>::iterator it = list.begin();
        int size = list.size();
        int i = 0;
        while (i < size) {
            // ### consider listing absoluteFilePath and storing that in
            // ### Location for quicker comparison
            if (list.at(i).location.fileName == absoluteFilePath) {
                list.removeAt(i);
            } else {
                ++i;
            }
        }
    }
}

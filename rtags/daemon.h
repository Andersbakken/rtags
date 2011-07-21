#ifndef DAEMON_H
#define DAEMON_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QHash>
#include <clang-c/Index.h>

class Daemon : public QObject
{
    Q_OBJECT
public:
    Daemon(QObject* parent = 0);
    ~Daemon();

    bool start();

private:
    QString addSourceFile(const QStringList& args);
    QString lookupLine(const QStringList& args);

private:
    Q_INVOKABLE QString runCommand(const QStringList& args);

private:
    CXIndex m_index;
    QHash<QString, CXTranslationUnit> m_translationUnits;
};

#endif

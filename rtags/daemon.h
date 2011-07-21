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
    Q_INVOKABLE QString runCommand(const QStringList& args);
private:
    QString addMakefile(const QString& path, const QStringList& args);
    QString addSourceFile(const QStringList& args);
    QString removeSourceFile(const QStringList& args);
    QString lookupLine(const QStringList& args);

private:
    bool addMakefileLine(const QList<QByteArray>& line);

private:
    CXIndex m_index;
    QHash<QString, CXTranslationUnit> m_translationUnits;
};

#endif

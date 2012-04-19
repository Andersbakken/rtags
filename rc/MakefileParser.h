#ifndef MAKEFILEPARSER_H
#define MAKEFILEPARSER_H

#include "Path.h"
#include "GccArguments.h"
#include <QObject>
#include <QList>

class QProcess;
class DirectoryTracker;

class MakefileParser : public QObject
{
    Q_OBJECT
public:
    MakefileParser(const QList<QByteArray> &extraFlags, QObject *parent = 0);
    ~MakefileParser();

    void run(const Path& makefile);
    bool isDone() const;
    QList<QByteArray> extraFlags() const { return mExtraFlags; }
signals:
    void done();
    void fileReady(const GccArguments& args);

private slots:
    void processMakeOutput();

private:
    void processMakeLine(const QByteArray& line);

private:
    QProcess* mProc;
    QByteArray mData;
    DirectoryTracker* mTracker;
    const QList<QByteArray> mExtraFlags;
};

#endif // MAKEFILEPARSER_H

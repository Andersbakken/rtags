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
    enum Verbosity { Silent, Verbose };

    MakefileParser(Verbosity verbosity = Silent, QObject *parent = 0);
    ~MakefileParser();

    void run(const Path& makefile);
    bool isDone() const;
signals:
    void done();
    void fileReady(const GccArguments& args);

private slots:
    void processMakeOutput();

private:
    void processMakeLine(const QByteArray& line);

private:
    bool mVerbose;
    QProcess* mProc;
    QByteArray mData;
    DirectoryTracker* mTracker;
};

#endif // MAKEFILEPARSER_H

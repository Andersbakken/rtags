#ifndef MAKEFILEPARSER_H
#define MAKEFILEPARSER_H

#include "GccArguments.h"
#include "Path.h"
#include <QObject>

class QProcess;
class DirectoryTracker;

struct MakefileItem
{
    Path filename;
    GccArguments arguments;
};

class MakefileParser : public QObject
{
    Q_OBJECT
public:
    MakefileParser(QObject *parent = 0);
    ~MakefileParser();

    void run(const Path& makefile);

signals:
    void done();
    void fileReady(const MakefileItem& makefile);

private slots:
    void processMakeOutput();

private:
    void processMakeLine(const QByteArray& line);

private:
    QProcess* mProc;
    QByteArray mData;
    DirectoryTracker* mTracker;
};

#endif // MAKEFILEPARSER_H

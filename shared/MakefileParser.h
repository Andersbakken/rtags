#ifndef MAKEFILEPARSER_H
#define MAKEFILEPARSER_H

#include "Path.h"
#include "GccArguments.h"
#include <QObject>
#include <QList>
#include <Hash.h>
#include <QProcess>

class DirectoryTracker;

class MakefileParser : public QObject
{
    Q_OBJECT
public:
    MakefileParser(const QList<ByteArray> &extraFlags, QObject *parent = 0);
    ~MakefileParser();

    void run(const Path &makefile, const QList<ByteArray> &args);
    bool isDone() const;
    QList<ByteArray> extraFlags() const { return mExtraFlags; }
    QList<ByteArray> mapPchToInput(const QList<ByteArray> &input) const;
    void setPch(const ByteArray &output, const ByteArray &input);
    int pchCount() const { return mPchs.size(); }
signals:
    void done();
    void fileReady(const GccArguments &args);

private slots:
    void processMakeOutput();
    void onReadyReadStandardError();
    void onError(QProcess::ProcessError error);
    void onProcessStateChanged(QProcess::ProcessState state);
private:
    void processMakeLine(const ByteArray &line);

private:
    QProcess *mProc;
    ByteArray mData;
    DirectoryTracker *mTracker;
    const QList<ByteArray> mExtraFlags;
    Hash<ByteArray, ByteArray> mPchs;
};

#endif // MAKEFILEPARSER_H

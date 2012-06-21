#ifndef MAKEFILEPARSER_H
#define MAKEFILEPARSER_H

#include "Path.h"
#include "GccArguments.h"
#include <QObject>
#include <List.h>
#include <Map.h>
#include <QProcess>

class DirectoryTracker;

class MakefileParser : public QObject
{
    Q_OBJECT
public:
    MakefileParser(const List<ByteArray> &extraFlags, QObject *parent = 0);
    ~MakefileParser();

    void run(const Path &makefile, const List<ByteArray> &args);
    bool isDone() const;
    List<ByteArray> extraFlags() const { return mExtraFlags; }
    List<ByteArray> mapPchToInput(const List<ByteArray> &input) const;
    void setPch(const ByteArray &output, const ByteArray &input);
    Path makefile() const { return mMakefile; }
signals:
    void done(int sources, int pchs);
    void fileReady(const GccArguments &args);

private slots:
    void onDone();
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
    const List<ByteArray> mExtraFlags;
    Map<ByteArray, ByteArray> mPchs;
    int mSourceCount, mPchCount;
    Path mMakefile;
};

#endif // MAKEFILEPARSER_H

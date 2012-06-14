#ifndef MAKEFILEPARSER_H
#define MAKEFILEPARSER_H

#include "Path.h"
#include "GccArguments.h"
#include <QObject>
#include <QList>
#include <QProcess>

class DirectoryTracker;

class MakefileParser : public QObject
{
    Q_OBJECT
public:
    MakefileParser(const QList<QByteArray> &extraFlags, QObject *parent = 0);
    ~MakefileParser();

    void run(const Path &makefile, const QList<QByteArray> &args);
    bool isDone() const;
    QList<QByteArray> extraFlags() const { return mExtraFlags; }
    QList<QByteArray> mapPchToInput(const QList<QByteArray> &input) const;
    void setPch(const QByteArray &output, const QByteArray &input);
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
    void processMakeLine(const QByteArray &line);

private:
    QProcess *mProc;
    QByteArray mData;
    DirectoryTracker *mTracker;
    const QList<QByteArray> mExtraFlags;
    QHash<QByteArray, QByteArray> mPchs;
};

#endif // MAKEFILEPARSER_H

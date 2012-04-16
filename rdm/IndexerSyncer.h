#ifndef IndexerSyncer_h
#define IndexerSyncer_h

#include "Indexer.h"
#include <QtCore>

class IndexerSyncer : public QThread
{
public:
    IndexerSyncer(QObject* parent = 0);

    void addSymbols(const SymbolHash &data);
    void addSymbolNames(const SymbolNameHash &symbolNames);
    void addDependencies(const DependencyHash& dependencies);
    void setPchDependencies(const DependencyHash& dependencies);
    void addFileInformation(const Path& input, const QList<QByteArray>& args, time_t timeStamp);
    void notify();
    void stop();

protected:
    void run();

private:
    bool mStopped;
    QMutex mMutex;
    QWaitCondition mCond;
    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;
    DependencyHash mDependencies, mPchDependencies;
    InformationHash mInformations;
};

#endif

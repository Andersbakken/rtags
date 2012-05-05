#ifndef IndexerSyncer_h
#define IndexerSyncer_h

#include "Indexer.h"
#include <QtCore>

class IndexerSyncer : public QThread
{
    Q_OBJECT
public:
    IndexerSyncer(QObject* parent = 0);

    void addSymbols(const SymbolHash &data);
    void addSymbolNames(const SymbolNameHash &symbolNames);
    void addDependencies(const DependencyHash& dependencies);
    void setPchDependencies(const DependencyHash& dependencies);
    void addFileInformation(const Path& input, const QList<QByteArray>& args, time_t timeStamp);
    void addFileInformations(const QSet<Path>& files);
    void addReferences(const ReferenceHash &references);
    void addPchUSRHash(const Path &pchHeader, const PchUSRHash &hash);
    void notify();
    void stop();
    void wait();

protected:
    void run();
signals:
    void symbolNamesChanged();
private:
    void maybeWake();

    bool mStopped;
    QMutex mMutex;
    QWaitCondition mCond, mIndexerJobCondition;
    SymbolHash mSymbols;
    SymbolNameHash mSymbolNames;
    DependencyHash mDependencies, mPchDependencies;
    InformationHash mInformations;
    ReferenceHash mReferences;
    QHash<Path, PchUSRHash> mPchUSRHashes;
};

#endif

#include "IndexerSyncer.h"
#include "LevelDB.h"

IndexerSyncer::IndexerSyncer(QObject* parent)
    : QThread(parent), mStopped(false)
{
}

void IndexerSyncer::stop()
{
    QMutexLocker locker(&mMutex);
    mStopped = true;
    mCond.wakeOne();
}

void IndexerSyncer::notify()
{
    QMutexLocker locker(&mMutex); // is this needed here?
    mCond.wakeOne();
}

void IndexerSyncer::addSymbolNames(const SymbolNameHash &locations)
{
    QMutexLocker lock(&mMutex);
    if (mSymbolNames.isEmpty()) {
        mSymbolNames = locations;
    } else {
        const SymbolNameHash::const_iterator end = locations.end();
        for (SymbolNameHash::const_iterator it = locations.begin(); it != end; ++it) {
            mSymbolNames[it.key()].unite(it.value());
        }
    }
}

void IndexerSyncer::addSymbols(const SymbolHash &symbols)
{
    QMutexLocker lock(&mMutex);
    if (mSymbols.isEmpty()) {
        mSymbols = symbols;
    } else {
        const SymbolHash::const_iterator end = symbols.end();
        for (SymbolHash::const_iterator it = symbols.begin(); it != end; ++it) {
            mSymbols[it.key()].unite(it.value());
        }
    }
}

void IndexerSyncer::addDependencies(const DependencyHash& dependencies)
{
    QMutexLocker lock(&mMutex);
    if (mDependencies.isEmpty()) {
        mDependencies = dependencies;
    } else {
        const DependencyHash::const_iterator end = dependencies.end();
        for (DependencyHash::const_iterator it = dependencies.begin(); it != end; ++it) {
            mDependencies[it.key()].unite(it.value());
        }
    }
}

void IndexerSyncer::setPchDependencies(const DependencyHash& dependencies)
{
    QMutexLocker lock(&mMutex);
    if (mPchDependencies.isEmpty()) {
        mPchDependencies = dependencies;
    } else {
        const DependencyHash::const_iterator end = dependencies.end();
        for (DependencyHash::const_iterator it = dependencies.begin(); it != end; ++it) {
            mPchDependencies[it.key()].unite(it.value());
        }
    }
}


void IndexerSyncer::addFileInformation(const Path& input, const QList<QByteArray>& args)
{
    QMutexLocker lock(&mMutex);
    mInformations[input] = args;
}

void IndexerSyncer::run()
{
    while (true) {
        SymbolNameHash symbolNames;
        SymbolHash symbols;
        DependencyHash dependencies, pchDependencies;
        InformationHash informations;
        {
            QMutexLocker locker(&mMutex);
            if (mStopped)
                return;
            while (mSymbols.isEmpty()
                   && mSymbolNames.isEmpty()
                   && mDependencies.isEmpty()
                   && mInformations.isEmpty()
                   && mPchDependencies.isEmpty()) {
                mCond.wait(&mMutex, 10000);
                if (mStopped)
                    return;

            }
            qSwap(symbolNames, mSymbolNames);
            qSwap(symbols, mSymbols);
            qSwap(dependencies, mDependencies);
            qSwap(pchDependencies, mPchDependencies);
            qSwap(informations, mInformations);
        }
        if (!symbolNames.isEmpty()) {
            LevelDB db;
            if (!db.open(Database::SymbolName, LevelDB::ReadWrite))
                return;

            leveldb::WriteBatch batch;

            SymbolNameHash::iterator it = symbolNames.begin();
            const SymbolNameHash::const_iterator end = symbolNames.end();
            bool changed = false;
            while (it != end) {
                const char *key = it.key().constData();
                const QSet<RTags::Location> added = it.value();
                QSet<RTags::Location> current = Rdm::readValue<QSet<RTags::Location> >(db.db(), key);
                const int oldSize = current.size();
                current += added;
                if (current.size() != oldSize) {
                    changed = true;
                    Rdm::writeValue<QSet<RTags::Location> >(&batch, key, current);
                }
                ++it;
            }

            if (changed)
                db.db()->Write(leveldb::WriteOptions(), &batch);
        }
        if (!symbols.isEmpty()) {
            LevelDB db;
            if (!db.open(Database::Symbol, LevelDB::ReadWrite))
                return;

            leveldb::WriteBatch batch;

            SymbolHash::iterator it = symbols.begin();
            const SymbolHash::const_iterator end = symbols.end();
            bool changed = false;
            while (it != end) {
                const QByteArray key = it.key().key(RTags::Location::Padded);
                Rdm::CursorInfo added = it.value();
                Rdm::CursorInfo current = Rdm::readValue<Rdm::CursorInfo>(db.db(), key.constData());
                if (current.unite(added)) {
                    changed = true;
                    Rdm::writeValue<Rdm::CursorInfo>(&batch, key, current);
                }
                ++it;
            }

            if (changed)
                db.db()->Write(leveldb::WriteOptions(), &batch);
        }
        if (!dependencies.isEmpty()) {
            LevelDB db;
            if (!db.open(Database::Dependency, LevelDB::ReadWrite))
                return;

            leveldb::WriteBatch batch;

            DependencyHash::iterator it = dependencies.begin();
            const DependencyHash::const_iterator end = dependencies.end();
            bool changed = false;
            while (it != end) {
                const char* key = it.key().constData();
                QSet<Path> added = it.value();
                QSet<Path> current = Rdm::readValue<QSet<Path> >(db.db(), key);
                const int oldSize = current.size();
                if (current.unite(added).size() > oldSize) {
                    changed = true;
                    Rdm::writeValue<QSet<Path> >(&batch, key, current);
                }
                ++it;
            }

            if (changed)
                db.db()->Write(leveldb::WriteOptions(), &batch);
        }
        if (!pchDependencies.isEmpty()) {
            LevelDB db;
            if (!db.open(Database::Dependency, LevelDB::ReadWrite))
                return;
            Rdm::writeValue<DependencyHash>(db.db(), "pch", pchDependencies);
        }
        if (!informations.isEmpty()) {
            leveldb::WriteBatch batch;

            InformationHash::iterator it = informations.begin();
            const InformationHash::const_iterator end = informations.end();
            while (it != end) {
                const char *key = it.key().constData();
                Rdm::writeValue<QList<QByteArray> >(&batch, key, it.value());
                ++it;
            }
            LevelDB db;
            if (!db.open(Database::FileInformation, LevelDB::ReadWrite))
                return;

            db.db()->Write(leveldb::WriteOptions(), &batch);
        }
    }
}

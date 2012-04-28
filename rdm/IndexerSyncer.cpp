#include "IndexerSyncer.h"
#include "leveldb/db.h"
#include "Server.h"

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
    maybeWake();
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
    maybeWake();
}

void IndexerSyncer::addReferences(const ReferenceHash &references)
{
    QMutexLocker lock(&mMutex);
    if (mReferences.isEmpty()) {
        mReferences = references;
    } else {
        const ReferenceHash::const_iterator end = references.end();
        for (ReferenceHash::const_iterator it = references.begin(); it != end; ++it) {
            mReferences[it.key()] = it.value();
        }
    }
    maybeWake();
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
    maybeWake();
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
    maybeWake();
}

void IndexerSyncer::addPchUSRHash(const Path &pchHeader, const PchUSRHash &hash)
{
    QMutexLocker lock(&mMutex);
    mPchUSRHashes[pchHeader] = hash;
    maybeWake();
}

void IndexerSyncer::addFileInformation(const Path& input, const QList<QByteArray>& args, time_t timeStamp)
{
    FileInformation fi;
    fi.lastTouched = timeStamp;
    fi.compileArgs = args;
    QMutexLocker lock(&mMutex);
    mInformations[input] = fi;
    maybeWake();
}

void IndexerSyncer::addFileInformations(const QSet<Path>& files)
{
    QMutexLocker lock(&mMutex);
    foreach (const Path &path, files) {
        FileInformation &fi = mInformations[path]; // force creation
        (void)fi;
    }
    maybeWake();
}


template <typename Container, typename Value>
static inline bool addTo(Container &container, const Value &value)
{
    const int oldSize = container.size();
    container += value;
    return container.size() != oldSize;
}

class Batch
{
public:
    enum { BatchThreshold = 1024 * 1024 };
    Batch(leveldb::DB *d)
        : db(d), batchSize(0), totalWritten(0)
    {}

    ~Batch()
    {
        write();
    }

    void write()
    {
        if (batchSize) {
            // error("About to write %d bytes to %p", batchSize, db);
            db->Write(leveldb::WriteOptions(), &batch);
            totalWritten += batchSize;
            // error("Wrote %d (%d) to %p", batchSize, totalWritten, db);
            batchSize = 0;
            batch.Clear();
        }
    }

    template <typename T>
    void add(const char *key, const T &t)
    {
        batchSize += Rdm::writeValue<T>(&batch, key, t);
        if (batchSize >= BatchThreshold)
            write();
    }

    leveldb::DB *db;
    leveldb::WriteBatch batch;
    int batchSize, totalWritten;
};

void IndexerSyncer::run()
{
    bool wroteSymbolNames = false;
    while (true) {
        SymbolNameHash symbolNames;
        SymbolHash symbols;
        DependencyHash dependencies, pchDependencies;
        InformationHash informations;
        ReferenceHash references;
        QHash<Path, PchUSRHash> pchUSRHashes;
        {
            QMutexLocker locker(&mMutex);
            if (mStopped)
                return;
            if (wroteSymbolNames && mSymbolNames.isEmpty()) {
                wroteSymbolNames = false;
                emit symbolNamesChanged();
            }
            while (mSymbols.isEmpty()
                   && mSymbolNames.isEmpty()
                   && mDependencies.isEmpty()
                   && mInformations.isEmpty()
                   && mReferences.isEmpty()
                   && mPchDependencies.isEmpty()
                   && mPchUSRHashes.isEmpty()) {
                mCond.wait(&mMutex, 10000);
                if (mStopped)
                    return;
            }
            qSwap(symbolNames, mSymbolNames);
            qSwap(symbols, mSymbols);
            qSwap(dependencies, mDependencies);
            qSwap(pchDependencies, mPchDependencies);
            qSwap(pchUSRHashes, mPchUSRHashes);
            qSwap(informations, mInformations);
            qSwap(references, mReferences);
        }
        warning() << "IndexerSyncer::run woke up symbols" << symbols.size()
                  << "symbolNames" << symbolNames.size()
                  << "dependencies" << dependencies.size()
                  << "informations" << informations.size()
                  << "references" << references.size()
                  << "pchDependencies" << pchDependencies.size()
                  << "pchUSRHashes" << pchUSRHashes.size();
        QList<QByteArray> out;

        if (!symbolNames.isEmpty()) {
            QElapsedTimer timer;
            timer.start();
            leveldb::DB *db = Server::instance()->db(Server::SymbolName);

            Batch batch(db);

            SymbolNameHash::iterator it = symbolNames.begin();
            const SymbolNameHash::const_iterator end = symbolNames.end();
            while (it != end) {
                const char *key = it.key().constData();
                const QSet<Location> added = it.value();
                QSet<Location> current = Rdm::readValue<QSet<Location> >(db, key);
                if (addTo(current, added)) {
                    batch.add(key, current);
                }
                ++it;
            }

            batch.write();
            if (batch.totalWritten) {
                out += QByteArray("Wrote " + QByteArray::number(symbolNames.size()) + " symbolNames "
                                  + QByteArray::number(batch.totalWritten) + " bytes in "
                                  + QByteArray::number(timer.elapsed()) + "ms");
                wroteSymbolNames = true;
            }
        }
        if (!references.isEmpty() || !symbols.isEmpty()) {
            QElapsedTimer timer;
            timer.start();
            leveldb::DB *symbolDB = Server::instance()->db(Server::Symbol);

            Batch batch(symbolDB);

            if (!references.isEmpty()) {
                const ReferenceHash::const_iterator end = references.end();
                for (ReferenceHash::const_iterator it = references.begin(); it != end; ++it) {
                    const SymbolHash::iterator sym = symbols.find(it.value().first);
                    if (sym != symbols.end()) {
                        CursorInfo &ci = sym.value();
                        ci.references.insert(it.key());
                        // if (it.value().first.path.contains("RTags.h"))
                        //     error() << "cramming" << it.key() << "into" << it.value();
                        if (it.value().second != Rdm::NormalReference) {
                            CursorInfo &other = symbols[it.key()];
                            ci.references += other.references;
                            other.references += ci.references;
                            if (other.target.isNull())
                                other.target = it.value().first;
                            if (ci.target.isNull())
                                ci.target = it.key();
                        }
                    } else {
                        const QByteArray key = it.value().first.key(Location::Padded);
                        CursorInfo current = Rdm::readValue<CursorInfo>(symbolDB, key.constData());
                        bool changedCurrent = false;
                        if (addTo(current.references, it.key()))
                            changedCurrent = true;
                        if (it.value().second != Rdm::NormalReference) {
                            const QByteArray otherKey = it.key().key(Location::Padded);
                            CursorInfo other = Rdm::readValue<CursorInfo>(symbolDB, otherKey);
                            bool changedOther = false;
                            if (addTo(other.references, it.key()))
                                changedOther = true;
                            if (addTo(other.references, current.references))
                                changedOther = true;
                            if (addTo(current.references, other.references))
                                changedCurrent = true;

                            if (other.target.isNull()) {
                                other.target = it.value().first;
                                changedOther = true;
                            }

                            if (current.target.isNull()) {
                                current.target = it.key();
                                changedCurrent = true;
                            }

                            if (changedOther) {
                                batch.add(otherKey, other);
                            }
                            // error() << "ditched reference" << it.key() << it.value();
                        }
                        if (changedCurrent) {
                            batch.add(key, current);
                        }
                    }
                }
            }
            if (!symbols.isEmpty()) {
                SymbolHash::iterator it = symbols.begin();
                const SymbolHash::const_iterator end = symbols.end();
                while (it != end) {
                    const QByteArray key = it.key().key(Location::Padded);
                    CursorInfo added = it.value();
                    bool ok;
                    CursorInfo current = Rdm::readValue<CursorInfo>(symbolDB, key.constData(), &ok);
                    if (!ok) {
                        batch.add(key, added);
                    } else if (current.unite(added)) {
                        batch.add(key, current);
                    }
                    ++it;
                }
            }
            batch.write();
            if (batch.totalWritten) {
                out += QByteArray("Wrote " + QByteArray::number(symbols.size())
                                  + " symbols and " + QByteArray::number(references.size())
                                  + " references " + QByteArray::number(batch.totalWritten) + " bytes in "
                                  + QByteArray::number(timer.elapsed()) + "ms");
            }
        }

        if (!dependencies.isEmpty()) {
            QElapsedTimer timer;
            timer.start();
            leveldb::DB *db = Server::instance()->db(Server::Dependency);
            Batch batch(db);

            DependencyHash::iterator it = dependencies.begin();
            const DependencyHash::const_iterator end = dependencies.end();
            while (it != end) {
                const char* key = it.key().constData();
                QSet<Path> added = it.value();
                QSet<Path> current = Rdm::readValue<QSet<Path> >(db, key);
                const int oldSize = current.size();
                if (current.unite(added).size() > oldSize) {
                    batch.add(key, current);
                }
                ++it;
            }
            batch.write();
            if (batch.totalWritten) {
                out += QByteArray("Wrote " + QByteArray::number(dependencies.size())
                                  + " dependencies, " + QByteArray::number(batch.totalWritten) + " bytes in "
                                  + QByteArray::number(timer.elapsed()) + "ms");
            }
        }
        if (!pchDependencies.isEmpty() || !pchUSRHashes.isEmpty()) {
            QElapsedTimer timer;
            timer.start();
            leveldb::DB *db = Server::instance()->db(Server::PCH);
            Batch batch(db);
            if (!pchDependencies.isEmpty())
                batch.add("dependencies", pchDependencies);

            for (QHash<Path, PchUSRHash>::const_iterator it = pchUSRHashes.begin(); it != pchUSRHashes.end(); ++it) {
                batch.add(it.key(), it.value());
            }
            batch.write();
            out += ("Wrote " + QByteArray::number(pchDependencies.size() + pchUSRHashes.size()) + " pch infos, "
                    + QByteArray::number(batch.totalWritten) + " bytes in "
                    + QByteArray::number(timer.elapsed()) + "ms");
        }
        if (!informations.isEmpty()) {
            QElapsedTimer timer;
            timer.start();
            leveldb::DB *db = Server::instance()->db(Server::FileInformation);
            Batch batch(db);

            InformationHash::iterator it = informations.begin();
            const InformationHash::const_iterator end = informations.end();
            while (it != end) {
                const char *key = it.key().constData();
                batch.add(key, it.key());
                ++it;
            }

            out += ("Wrote " + QByteArray::number(informations.size()) + " fileinfos, "
                    + QByteArray::number(batch.totalWritten) + " bytes in "
                    + QByteArray::number(timer.elapsed()) + "ms");
        }
        if (!out.isEmpty())
            error() << RTags::join(out, ", ").constData();
    }
}

void IndexerSyncer::maybeWake()
{
    const int size = (mSymbols.size() + mSymbolNames.size() + mDependencies.size() + mPchDependencies.size()
                      + mInformations.size() + mReferences.size() + mPchUSRHashes.size());
    enum { MaxSize = 1024 * 64 };
    if (size > MaxSize) // ### tunable?
        mCond.wakeOne();
}

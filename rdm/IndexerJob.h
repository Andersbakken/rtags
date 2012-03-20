#ifndef IndexerJob_h
#define IndexerJob_h

#include <QObject>
#include <QRunnable>
#include <QList>
#include <QByteArray>
#include "Indexer.h"
#include <QHash>
#include <QSet>

class IndexerJob : public QObject, public QRunnable
{
    Q_OBJECT
public:
    IndexerJob(IndexerImpl* impl, Indexer::Type type, Indexer::Mode mode, int id,
               const QByteArray& path, const QByteArray& input, const QList<QByteArray>& arguments);

    int id() const { return m_id; }

    void run();

    Indexer::Type m_type;
    Indexer::Mode m_mode;
    int m_id;
    QByteArray m_path, m_in;
    QList<QByteArray> m_args;
    IndexerImpl* m_impl;

    HashSet m_defs, m_refs, m_syms;

private:
    void addFileNameSymbol(const QByteArray& fileName);

signals:
    void done(int id, const QByteArray& input, const QByteArray& output);
};

#endif

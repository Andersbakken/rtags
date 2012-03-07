#ifndef INDEXER_H
#define INDEXER_H

#include <QObject>
#include <QByteArray>
#include <QList>

class IndexerImpl;

class Indexer : public QObject
{
    Q_OBJECT
public:
    enum Mode { None, Force };

    Indexer(const QByteArray& path, QObject* parent = 0);
    ~Indexer();

    int index(const QByteArray& input, const QList<QByteArray>& arguments, Mode mode = None);
    int reindex(const QByteArray& input, Mode mode = None);

    static Indexer* instance();

signals:
    void indexingDone(int id);

private slots:
    void jobDone(int id, const QByteArray& filename);

private:
    IndexerImpl* m_impl;
    static Indexer* s_inst;
};

#endif

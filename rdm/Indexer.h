#ifndef INDEXER_H
#define INDEXER_H

#include <QObject>
#include <QByteArray>
#include <QList>
#include <AddMessage.h>

class IndexerImpl;

class Indexer : public QObject
{
    Q_OBJECT
public:

    Indexer(const QByteArray& path, QObject* parent = 0);
    ~Indexer();

    int index(const QByteArray& input, const QList<QByteArray>& arguments);

    static Indexer* instance();

signals:
    void indexingDone(int id);

private slots:
    void jobDone(int id, const QByteArray& input);

private:
    IndexerImpl* m_impl;
    static Indexer* s_inst;
};

#endif

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
    enum Type { C, CPlusPlus };

    Indexer(const QByteArray& path, QObject* parent = 0);
    ~Indexer();

    int index(Type type, const QByteArray& input, const QByteArray&output,
              const QList<QByteArray>& arguments, Mode mode = None);

    static Indexer* instance();

signals:
    void indexingDone(int id);

private slots:
    void jobDone(int id, const QByteArray& input, const QByteArray& output);

private:
    IndexerImpl* m_impl;
    static Indexer* s_inst;
};

#endif

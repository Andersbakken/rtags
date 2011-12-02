#ifndef MMAP_H
#define MMAP_H

#include <QByteArray>
#include <QVector>

class Mmap
{
public:
    Mmap();
    Mmap(const QByteArray& filename);
    ~Mmap();

    bool load(const QByteArray& filename);
    void sync();

    bool isValid() const { return m_size > 0; }
    quint64 size() const { return m_size; }

    const QByteArray data(quint64 offset, unsigned int size) const;
    void update(quint64 offset, const QByteArray& data) { update(offset, data.constData(), data.size()); }
    void update(quint64 offset, const char* data, unsigned int size);

    QByteArray lastError() const { return m_error; }

private:
    void clear();

private:
    struct Data
    {
        char* data;
        unsigned int size;
    };

    QVector<Data> m_data;
    quint64 m_size;
    unsigned int m_bpd;
    QByteArray m_error;

private:
    Mmap(const Mmap& other);
    Mmap& operator=(const Mmap& other);
};

#endif // MMAP_H

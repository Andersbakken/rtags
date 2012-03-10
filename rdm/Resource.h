#ifndef RESOURCE_H
#define RESOURCE_H

#include <QByteArray>
#include <QList>

class Resource
{
public:
    enum Type { Information, AST };
    enum WriteMode { Normal, Truncate };
    enum LockMode { NoLock, Lock };

    Resource();
    Resource(const QByteArray &fileName, LockMode mode = Lock);
    ~Resource();

    void lock();
    void unlock();

    QByteArray fileName() const;
    void setFileName(const QByteArray& fileName, LockMode mode = Lock);
    bool exists(Type type) const;
    QByteArray hashedFileName(Type type) const;

    template<typename T>
    T read(Type type) const { return readData(type); }

    template<typename T>
    void write(Type type, const T& data, WriteMode mode = Normal) { writeData(type, data, mode); }

    static QByteArray hash(const QByteArray& fileName);
    static void setBaseDirectory(const QByteArray& base);

    void erase(Type type);
    void eraseAll();

protected:
    void writeData(Type type, const QByteArray& data, WriteMode mode);
    QByteArray readData(Type type) const;

private:
    static QByteArray s_base;
    QByteArray m_fileName, m_hash;
    bool m_locked;
};

template<>
inline QList<QByteArray> Resource::read(Type type) const
{
    return readData(type).split('\n');
}

template<>
inline void Resource::write(Type type, const QList<QByteArray>& data, WriteMode mode)
{
    QByteArray w;
    foreach(const QByteArray& entry, data) {
        w += entry + "\n";
    }
    writeData(type, w, mode);
}

#endif

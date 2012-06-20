#ifndef Map_h
#define Map_h

#include <map>
#include <List.h>

template <typename Key, typename Value>
class Map : public std::map<Key, Value>
{
public:
    Map() {}

    bool contains(const Key &t) const
    {
        return find(t) != std::map<Key, Value>::end();
    }

    bool isEmpty() const
    {
        return !std::map<Key, Value>::size();
    }

    Value value(const Key &key, const Value &defaultValue = Value()) const
    {
        typename std::map<Key, Value>::const_iterator it = find(key);
        if (it == std::map<Key, Value>::end()) {
            return defaultValue;
        }
        return it->second;
    }

    bool remove(const Key &t)
    {
        typename std::map<Key, Value>::iterator it = find(t);
        if (it != std::map<Key, Value>::end()) {
            erase(it);
            return true;
        }
        return false;
    }

    Map<Key, Value> &unite(const Map<Key, Value> &other)
    {
        typename std::map<Key, Value>::iterator it = other.begin();
        while (it != other.end()) {
            std::map<Key, Value>::insert(*it);
            ++it;
        }
        return *this;
    }

    Map<Key, Value> &subtract(const Map<Key, Value> &other)
    {
        typename std::map<Key, Value>::iterator it = other.begin();
        while (it != other.end()) {
            erase(*it);
            ++it;
        }
        return *this;
    }

    Map<Key, Value> &operator+=(const Map<Key, Value> &other)
    {
        return unite(other);
    }

    Map<Key, Value> &operator-=(const Map<Key, Value> &other)
    {
        return subtract(other);
    }

    int size() const
    {
        return std::map<Key, Value>::size();
    }
};

template <typename Key, typename Value>
inline const Map<Key, Value> operator+(const Map<Key, Value> &l, const Map<Key, Value> &r)
{
    Map<Key, Value> ret = l;
    ret += r;
    return ret;
}

template <typename Key, typename Value>
inline const Map<Key, Value> operator-(const Map<Key, Value> &l, const Map<Key, Value> &r)
{
    Map<Key, Value> ret = l;
    ret -= r;
    return ret;
}

template <typename Key, typename Value>
inline QDataStream &operator<<(QDataStream &ds, const Map<Key, Value> &hash)
{
    ds << hash.size();
    for (typename Map<Key, Value>::const_iterator it = hash.begin(); it != hash.end(); ++it) {
        ds << it->first << it->second;
    }
    return ds;
}

template <typename Key, typename Value>
inline QDataStream &operator>>(QDataStream &ds, Map<Key, Value> &hash)
{
    int size;
    ds >> size;
    Key t;
    Value v;
    for (int i=0; i<size; ++i) {
        ds >> t >> v;
        hash[t] = v;
    }
    return ds;
}

#endif

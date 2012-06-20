#ifndef Hash_h
#define Hash_h

#include <boost/unordered_map.hpp>
#include <QList>

template <typename Key, typename Value>
class Hash : public boost::unordered_map<Key, Value, boost::hash<Key> >
{
public:
    Hash() {}

    bool contains(const Key &t) const
    {
        return find(t) != boost::unordered_map<Key, Value, boost::hash<Key> >::end();
    }

    bool isEmpty() const
    {
        return !boost::unordered_map<Key, Value, boost::hash<Key> >::size();
    }

    Value value(const Key &key, const Value &defaultValue = Value()) const
    {
        typename boost::unordered_map<Key, Value, boost::hash<Key> >::const_iterator it = find(key);
        if (it == boost::unordered_map<Key, Value, boost::hash<Key> >::end()) {
            return defaultValue;
        }
        return it->second;
    }

    bool remove(const Key &t)
    {
        typename boost::unordered_map<Key, Value, boost::hash<Key> >::iterator it = find(t);
        if (it != boost::unordered_map<Key, Value, boost::hash<Key> >::end()) {
            erase(it);
            return true;
        }
        return false;
    }

    Hash<Key, Value> &unite(const Hash<Key, Value> &other)
    {
        typename boost::unordered_map<Key, Value, boost::hash<Key> >::iterator it = other.begin();
        while (it != other.end()) {
            boost::unordered_map<Key, Value, boost::hash<Key> >::insert(*it);
            ++it;
        }
        return *this;
    }

    Hash<Key, Value> &subtract(const Hash<Key, Value> &other)
    {
        typename boost::unordered_map<Key, Value, boost::hash<Key> >::iterator it = other.begin();
        while (it != other.end()) {
            erase(*it);
            ++it;
        }
        return *this;
    }

    Hash<Key, Value> &operator+=(const Hash<Key, Value> &other)
    {
        return unite(other);
    }

    Hash<Key, Value> &operator-=(const Hash<Key, Value> &other)
    {
        return subtract(other);
    }

    int size() const
    {
        return boost::unordered_map<Key, Value, boost::hash<Key> >::size();
    }
};

template <typename Key, typename Value>
inline const Hash<Key, Value> operator+(const Hash<Key, Value> &l, const Hash<Key, Value> &r)
{
    Hash<Key, Value> ret = l;
    ret += r;
    return ret;
}

template <typename Key, typename Value>
inline const Hash<Key, Value> operator-(const Hash<Key, Value> &l, const Hash<Key, Value> &r)
{
    Hash<Key, Value> ret = l;
    ret -= r;
    return ret;
}

template <typename Key, typename Value>
inline QDataStream &operator<<(QDataStream &ds, const Hash<Key, Value> &hash)
{
    ds << hash.size();
    for (typename Hash<Key, Value>::const_iterator it = hash.begin(); it != hash.end(); ++it) {
        ds << it->first << it->second;
    }
    return ds;
}

template <typename Key, typename Value>
inline QDataStream &operator>>(QDataStream &ds, Hash<Key, Value> &hash)
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

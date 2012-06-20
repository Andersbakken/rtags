#ifndef Set_h
#define Set_h

#include <boost/unordered_set.hpp>
#include <List.h>

template <typename T>
class Set : public boost::unordered_set<T, boost::hash<T> >
{
public:
    Set() {}

    bool contains(const T &t) const
    {
        return find(t) != boost::unordered_set<T, boost::hash<T> >::end();
    }

    bool isEmpty() const
    {
        return !boost::unordered_set<T, boost::hash<T> >::size();
    }

    bool remove(const T &t)
    {
        typename boost::unordered_set<T, boost::hash<T> >::iterator it = find(t);
        if (it != boost::unordered_set<T, boost::hash<T> >::end()) {
            erase(it);
            return true;
        }
        return false;
    }
    List<T> toList() const
    {
        List<T> ret;
        typename boost::unordered_set<T, boost::hash<T> >::iterator it = boost::unordered_set<T, boost::hash<T> >::begin();
        while (it != boost::unordered_set<T, boost::hash<T> >::end()) {
            ret.append(*it);
            ++it;
        }
        return ret;
    }

    Set<T> &unite(const Set<T> &other)
    {
        typename boost::unordered_set<T, boost::hash<T> >::iterator it = other.begin();
        while (it != other.end()) {
            boost::unordered_set<T, boost::hash<T> >::insert(*it);
            ++it;
        }
        return *this;
    }

    Set<T> &subtract(const Set<T> &other)
    {
        typename boost::unordered_set<T, boost::hash<T> >::iterator it = other.begin();
        while (it != other.end()) {
            erase(*it);
            ++it;
        }
        return *this;
    }

    Set<T> &operator+=(const Set<T> &other)
    {
        return unite(other);
    }

    Set<T> &operator-=(const Set<T> &other)
    {
        return subtract(other);
    }

    int size() const
    {
        return boost::unordered_set<T, boost::hash<T> >::size();
    }
};

template <typename T>
inline const Set<T> operator+(const Set<T> &l, const Set<T> &r)
{
    Set<T> ret = l;
    ret += r;
    return ret;
}

template <typename T>
inline const Set<T> operator-(const Set<T> &l, const Set<T> &r)
{
    Set<T> ret = l;
    ret -= r;
    return ret;
}

template <typename T>
inline QDataStream &operator<<(QDataStream &ds, const Set<T> &set)
{
    ds << set.size();
    for (typename Set<T>::const_iterator it = set.begin(); it != set.end(); ++it) {
        ds << *it;
    }
    return ds;
}

template <typename T>
inline QDataStream &operator>>(QDataStream &ds, Set<T> &set)
{
    int size;
    ds >> size;
    T t;
    for (int i=0; i<size; ++i) {
        ds >> t;
        set.insert(t);
    }
    return ds;
}


#endif

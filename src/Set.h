#ifndef Set_h
#define Set_h

#include <set>
#include <List.h>

template <typename T>
class Set : public std::set<T>
{
public:
    Set() {}

    bool contains(const T &t) const
    {
        return std::set<T>::find(t) != std::set<T>::end();
    }

    bool isEmpty() const
    {
        return !std::set<T>::size();
    }

    bool remove(const T &t)
    {
        typename std::set<T>::iterator it = std::set<T>::find(t);
        if (it != std::set<T>::end()) {
            std::set<T>::erase(it);
            return true;
        }
        return false;
    }
    List<T> toList() const
    {
        List<T> ret;
        typename std::set<T>::iterator it = std::set<T>::begin();
        while (it != std::set<T>::end()) {
            ret.append(*it);
            ++it;
        }
        return ret;
    }

    bool insert(const T &t)
    {
        return std::set<T>::insert(t).second;
    }

    Set<T> &unite(const Set<T> &other, int *count = 0)
    {
        int c = 0;
        typename std::set<T>::iterator it = other.begin();
        while (it != other.end()) {
            if (insert(*it))
                ++c;
            ++it;
        }
        if (count)
            *count = c;
        return *this;
    }

    Set<T> &subtract(const Set<T> &other, int *count = 0)
    {
        int c = 0;
        typename std::set<T>::iterator it = other.begin();
        while (it != other.end()) {
            c += std::set<T>::erase(*it);
            ++it;
        }
        if (count)
            *count = c;
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
        return std::set<T>::size();
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

#endif

#ifndef List_h
#define List_h

#include <vector>
#include <algorithm>
#include <assert.h>

template <typename T> class Set;

template <typename T>
class List : public std::vector<T>
{
public:
    List(int count = 0, const T &defaultValue = T())
        : std::vector<T>(count, defaultValue)
    {}

    bool contains(const T &t) const
    {
        return std::find(std::vector<T>::begin(), std::vector<T>::end(), t) != std::vector<T>::end();
    }

    bool isEmpty() const
    {
        return std::vector<T>::empty();
    }

    void append(const T &t)
    {
        std::vector<T>::push_back(t);
    }

    void append(const List<T> &t)
    {
        const int size = t.size();
        for (int i=0; i<size; ++i)
            std::vector<T>::push_back(t.at(i));
    }

    void removeLast()
    {
        std::vector<T>::pop_back();
    }

    int size() const
    {
        return std::vector<T>::size();
    }

    T value(int idx, const T &defaultValue = T())
    {
        return idx < size() ? std::vector<T>::at(idx) : defaultValue;
    }

    void chop(int count)
    {
        assert(count <= size());
        std::vector<T>::resize(size() - count);
    }
    Set<T> toSet() const; // implemented in Set.h

    T &last()
    {
        return operator[](size() - 1);
    }

    const T &last() const
    {
        return at(size() - 1);
    }

    List<T> &operator+=(const T &t)
    {
        append(t);
        return *this;
    }

    List<T> &operator+=(const List<T> &t)
    {
        append(t);
        return *this;
    }
};

#endif

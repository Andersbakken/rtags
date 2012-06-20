#ifndef List_h
#define List_h

#include <vector>

template <typename T>
class List : public std::vector<T>
{
public:
    List() {}

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
        push_back(t);
    }

    void append(const List<T> &t)
    {
        const int size = t.size();
        for (int i=0; i<size; ++i)
            append(t.at(i));
    }

    int size() const
    {
        return std::vector<T>::size();
    }

    T value(int idx, const T &defaultValue = T())
    {
        return idx < size() ? std::vector<T>::at(idx) : defaultValue;
    }
};

#endif

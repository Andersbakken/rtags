#ifndef List_h
#define List_h

#include <vector>
#include <algorithm>
#include <assert.h>

template <typename T> class Set;

template <typename T>
class List : public std::vector<T>
{
    typedef std::vector<T> Base;
public:
    List(int count = 0, const T &defaultValue = T())
        : Base(count, defaultValue)
    {}

    template <typename CompatibleType>
    List(const std::vector<CompatibleType> &other)
        : Base(other.size(), T())
    {
        const int size = other.size();
        for (int i=0; i<size; ++i) {
            std::vector<T>::operator[](i) = other.at(i);
        }
    }

    bool contains(const T &t) const
    {
        return std::find(Base::begin(), Base::end(), t) != Base::end();
    }

    bool isEmpty() const
    {
        return Base::empty();
    }

    void append(const T &t)
    {
        Base::push_back(t);
    }

    void prepend(const T &t)
    {
        Base::insert(Base::begin(), t);
    }

    void append(const List<T> &t)
    {
        const int size = t.size();
        for (int i=0; i<size; ++i)
            Base::push_back(t.at(i));
    }

    int indexOf(const T &t) const
    {
        const typename Base::const_iterator beg = Base::begin();
        const typename Base::const_iterator end = Base::end();
        const typename Base::const_iterator it = std::find(beg, end, t);
        return it == end ? -1 : (it - beg);
    }

    int lasIndexOf(const T &t, int from = -1) const
    {
        const int s = size();
        if (from < 0) {
            from += s;
        }
        from = std::min(s - 1, from);
        if (from >= 0) {
            const T *haystack = Base::constData();
            const T *needle = haystack + from + 1;
            while (needle != haystack) {
                if (*--needle == t)
                    return needle - haystack;
            }
        }
        return -1;
    }

    void removeAt(int idx)
    {
        Base::erase(Base::begin() + idx);
    }

    void removeLast()
    {
        Base::pop_back();
    }

    int size() const
    {
        return Base::size();
    }

    T value(int idx, const T &defaultValue = T()) const
    {
        return idx < size() ? Base::at(idx) : defaultValue;
    }

    void chop(int count)
    {
        assert(count <= size());
        Base::resize(size() - count);
    }
    Set<T> toSet() const; // implemented in Set.h

    T &first()
    {
        return Base::operator[](0);
    }
    const T &first() const
    {
        return Base::at(0);
    }

    T &last()
    {
        return Base::operator[](size() - 1);
    }

    const T &last() const
    {
        return Base::at(size() - 1);
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

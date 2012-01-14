template <typename T>
T foo(T t)
{
    return t + t;
}

template <>
int foo(int t)
{
    return t * t;
}

template <typename T>
class A
{
public:
    A(T t)
        : mT(t)
    {}

    T t() const { return mT; }
private:
    const T mT;
};


int main()
{
    foo<int>(12);
    foo<char>('b');
    A<int> a(13);
    int b = a.t();
    return b;
}


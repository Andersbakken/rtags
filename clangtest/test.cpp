// #include <stdio.h>
template <typename T, typename TT>
class TTT
{
public:
    TTT(T, TT)
    {
        // printf("%s:%d TTT(T, TT)\n", __FILE__, __LINE__);
    }

    void foo()
    {

    }
};

template <typename T>
class TTT<T, int>
{
public:
    TTT(T t, int i)
    {
        // printf("%s:%d TTT(T t, int i)\n", __FILE__, __LINE__);
    }
};

class A
{
public:
    A() {}
};

// template <typename T, typename TT> T foo(T t, TT tt);

int main()
{
    TTT<char, char> t('a', 'b');
    TTT<char, int> tt('a', 12);
    TTT<char, double> ttt('a', 12.12);

    // int a = foo(13);
    // double b = foo(14);
    // A c = foo<A>(A());
    // int local;
    // // foobar();
    // N::A a;
    // a.member = 12;
    // return a.foo();
}

// template <typename T> T foo(T t)
// {
//     return t;
// }

// using namespace N;
// int A::foo()
// {
//     return member;
// }

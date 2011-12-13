// namespace N {
// class A
// {
// public:
//     A()
//         : member(13)
//     {}
//     int foo();
//     int member;
// };
// }

class A
{
public:
    A() {}
};

template <typename T> T foo(T t)
{
    return t;
}


// void foobar();
int main()
{
    int a = foo(13);
    double b = foo(14);
    A c = foo<A>(A());
    // int local;
    // // foobar();
    // N::A a;
    // a.member = 12;
    // return a.foo();
}
// using namespace N;
// int A::foo()
// {
//     return member;
// }

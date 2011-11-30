namespace N {
class A
{
public:
    A()
        : bar(13)
    {}
    int foo();
    int bar;
};
};

// void foobar();
int main()
{
    // foobar();
    N::A a;
    // a.bar = 12;
    return a.foo();
}
using namespace N;
int A::foo()
{
    return bar;
}

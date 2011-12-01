namespace N {
class A
{
public:
    A()
        : member(13)
    {}
    int foo();
    int member;
};
};

// void foobar();
int main()
{
    int local;
    // foobar();
    N::A a;
    a.member = 12;
    return a.foo();
}
using namespace N;
int A::foo()
{
    return member;
}

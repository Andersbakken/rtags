namespace NM {
class A;
class A
{
public:
    A()
        : oof(0)
    {}
    void foo();
    int oof;
};
};

class B : public NM::A
{
public:
    B();
    void bar()
    {
        A a;
        foo();
    }
};

B::B()
{
}

using namespace NM;
void A::foo()
{
    B b;
    b.bar();
}



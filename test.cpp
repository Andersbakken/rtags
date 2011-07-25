namespace NM {
class A
{
public:
    A() {}
    void foo();
};
};

class B : public NM::A
{
public:
    B();
    void bar()
    {
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



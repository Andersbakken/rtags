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
    inline void balle()
    {
        return inlineFunc(this);
    }
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
    int balle = 12;
    double bb = double(balle);
    B b;
    b.bar();
}



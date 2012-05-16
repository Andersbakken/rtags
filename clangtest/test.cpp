// #include "header.h"

class Outer
{
public:
    void foo();
    void inlinefunc()
    {}
    void inlinefunc2()
    {
        inlinefunc();
    }
    
};

void bar()
{
}

int main()
{
    // Enum e = Foo;
    Outer o;

    o.foo();
    return 0;
}

void Outer::foo()
{
    bar();
    foo();
}

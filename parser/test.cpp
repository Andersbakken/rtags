#include "header.h"

class Outer
{
public:
    unsigned long long foo();
    void inlinefunc()
    {}
    void inlinefunc2()
    {
        inlinefunc();
    }

    void inl() {}
};

void bar()
{
}

int main()
{
    // Enum e = Foo;
    Outer o;
    Outer *oo = &o;
    oo->foo();

    o.foo();
    return 0;
}

unsigned long long Outer::foo()
{
    bar();
    foo();
    // vil ikke ha dette

    char *foo = "12312312321";
    return 0;
}

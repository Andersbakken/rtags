#include "header.h"

class Outer
{
public:
    void foo();
};

void bar()
{
}

int main()
{
    Enum e = Foo;
    Outer o;

    o.foo();
    return 0;
}

void Outer::foo()
{
    bar();
    return;
}

// #include "foo.h"

class Foo
{
public:
    Foo(int);
    void foo();
};

Foo::Foo(int)
{
}

int main()
{
    char buf[1024];
    // strcpy(buf, "foo");
    // strcpy(buf, "foo");
    Foo f(12);
    f.foo();
    f.foo();
    return 0;
}

// int main()
// {
//     void *ptr = (void*)&main;
//     return main();
// }

void Foo::foo()
{

}

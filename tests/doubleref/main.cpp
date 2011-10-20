class Foo
{
public:
    Foo(int)
    {
    }
    void foo()
    {
    }
};

int main()
{
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

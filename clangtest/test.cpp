class A
{
public:
    A()
        : bar(13)
    {}
    int foo();
    int bar;
};

void foobar();
int main()
{
    foobar();
    A a;
    a.bar = 12;
    return a.foo();
}

int A::foo()
{
    return bar;
}

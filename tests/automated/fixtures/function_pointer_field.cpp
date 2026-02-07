struct Foo
{
    void (*foo)();
};


int main()
{
    Foo f;
    f.foo();
    return 0;
}

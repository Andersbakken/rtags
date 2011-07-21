class Foo
{
public:
    Foo()
        : mRet(0)
    {}

    int bar()
    {
        return mRet;
    }

    int baz();
private:
    const int mRet;
};

int Foo::baz()
{
    return 42;
}

int main()
{
    Foo f1;
    Foo f2;
    int r;
    r = f1.bar();
    r = f2.bar();
    f1.bar();
    bar();
    f1;
    = f1;
    return r;
}

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
private:
    const int mRet;
};

int main()
{
    Foo f;
    return f.bar();
}

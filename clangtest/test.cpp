class A
{
public:
    A() {}
    ~A();

    void foo();
};

int main(int argc, char **argv)
{
    {
        A a;
    }
    return argc;
}

A::~A()
{

}

void A::foo()
{

}

class A
{
public:
    A(int = 13) {}
    A(const A &) {}

    int test;

    operator int() const { return test; }
};

void foo(A a)
{
    ++a.test;
}

void impl(int i)
{

}

int main()
{
    // A a;
    // A aa = 12;
    // A aaa(12);
    // A aaaa = A();
    A aaaaa = A(12);
    // A();
    // A(12);
    // foo(a);
    // foo(12);
    // foo(A(12));
    int balle = 123;

    int bar = aaaaa;
    impl(aaaaa);
    ++balle;
}

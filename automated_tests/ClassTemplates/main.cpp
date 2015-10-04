struct A {
    void foo() {}
};
struct B {
    int foo() { return 0; }
};

template <typename T>
struct X {
    void f() {
        T t;
        t.foo();
    }
};

template struct X<A>;
template struct X<B>;

void use () {
    X<A> x;
}

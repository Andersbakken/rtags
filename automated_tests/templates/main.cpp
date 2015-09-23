struct A {
    void foo() {}
};
struct B {
    void foo() {}
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

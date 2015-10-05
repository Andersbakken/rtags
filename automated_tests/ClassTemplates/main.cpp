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


template <typename... Ts>
struct Y {};

template struct Y<int, int>;
template struct Y<int, char>;
template struct Y<char, int>;
template struct Y<char, char>;
template struct Y<char, char, char>;



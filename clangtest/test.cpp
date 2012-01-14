// template <typename T>
// class C
// {
// public:
//     C(const T &t)
//         : mT(t)
//     {}

//     T t() const { return mT; }
// private:
//     const T mT;
// };

class B
{
public:
    B();

    void foo();
};

template <typename T>
T foo(T t)
{
    return (t + t);
}

template <>
int foo(int f)
{
    return f * f;
}

int main()
{
    int a = foo(12);
    char b = foo('a');
    double c = foo(1.2);

    // C<int> cc(12);

    // int bb = cc.t();
    // return bb;
}

B::B()
{
}

void B::foo()
{

}

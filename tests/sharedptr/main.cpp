class A
{
public:
    A() {}
};

template <typename T>
class Pointer
{
public:
    Pointer()
    {}
    void reset(T *t)
    {}
};

class B
{
public:
    void send(Pointer<A> a)
    {

    }
};


int main()
{
    B b;
    Pointer<A> a;
    a.reset(new A);
    b.send(a);
}

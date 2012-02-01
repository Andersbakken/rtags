// #include <QByteArray>
// #include <QList>
// #include <qglobal.h>

template <typename T>
class List
{
public:
    List(const T &t)
        : tt(t)
    {}

    T func() const { return tt; }

private:
    T tt;
};

// class A
// {
// public:
//     A() {}

//     A *parent;
// };

// List<A> *ptr = 0;

// void func(List<A> *list)
// {
// }

int main()
{
    // A balle;
    // List<A> list(balle);
    // A aa = list.func();
    return 0;
    // QList<QByteArray> foo;
    // foreach(const QByteArray &f, foo) {
    //     return f.isEmpty() ? 0 : 1;
    // }
    // return 0;
}

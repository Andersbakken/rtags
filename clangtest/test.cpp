// enum Enum {
#define    Balle 0
#define    Foobar 1
// };

class Outer
{
public:

    class Inner
    {
    public:
        Inner(Outer *o);
    };
};

Outer::Inner::Inner(Outer *o)
{
    delete o;
}

int main()
{
    switch (2) {
    case Balle:
        break;
    case Foobar:
        break;
    }
    Outer *o = new Outer;
    Outer::Inner *i = new Outer::Inner(o);
    return 0;
}

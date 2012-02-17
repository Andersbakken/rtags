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
    Outer *o = new Outer;
    Outer::Inner *i = new Outer::Inner(o);
    return 0;
}

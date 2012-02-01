struct B
{
    int b;
};

template <typename T>
class TemplateClass
{
public:
    TemplateClass(T t) { int b = t; if (b > t) ++b; }

    void foo(const B &b);
};

class NonTemplateClass
{
public:
    NonTemplateClass() {}

    void foo(const B &b);
};

int main()
{
    TemplateClass<int> tt(12);
    NonTemplateClass t;
    return 0;
}

template <typename T>
void TemplateClass<T>::foo(const B &b)
{

}


void NonTemplateClass::foo(const B &b)
{

}

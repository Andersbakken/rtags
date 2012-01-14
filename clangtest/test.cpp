template <typename T>
class TemplateClass
{
public:
    TemplateClass(T t) { int b = t; if (b > t) ++b; }
};

class NonTemplateClass
{
public:
    NonTemplateClass() {}
};

int main()
{
    TemplateClass<int> tt(12);
    NonTemplateClass t;
    return 0;
}


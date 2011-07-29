#include "testtemplate.h"

struct Struct {
    int a;
};
namespace NM {

class Class
{
public:
    class InnerClass {
    public:
        template<typename T>
        static void bar(T t)
        {
            // foo(t);
        }
    };
    int outerFunc();
};
};

int main(int argc, char** argv)
{
    NM::Class::InnerClass::bar(1);
    NM::Class::InnerClass::bar('a');
    NM::Class::InnerClass::bar<const char*>("adasd");
    NM::Class cl;
    cl.outerFunc();
    // foo<const char*>("asdasd");
    return 0;
}

using namespace NM;

int Class::outerFunc()
{
    return 1;
}

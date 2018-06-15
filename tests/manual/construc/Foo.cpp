#include "Foo.h"

class B
{
public:
    void reallyInline()
    {

    }

    void sortaInline();
};

Foo::Foo()
{
    func();
}

void Foo::func()
{
}

void B::sortaInline()
{


}

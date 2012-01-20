#include "foo1.h"
#include "foo2.h"
#include "foo3.h"
#include "foo4.h"

int main()
{
    extern int foo1();
    extern int foo2();
    extern int foo3();
    extern int foo4();
    foo1();
    foo2();
    foo3();
    foo4();
    return 0;
}

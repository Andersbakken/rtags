// rc -e"-C -j1" -M Makefile -f /home/abakken/dev/rtags/construc/Foo.cpp,269 -q => should work, crashes during indexing

#include "Foo.h"

int main()
{
    Foo f;
    f.func();
    return 0;
}

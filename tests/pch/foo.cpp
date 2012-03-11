#include "foo.h"
#include <stdio.h>

extern "C"
void foo()
{
    printf("foo\n");
}

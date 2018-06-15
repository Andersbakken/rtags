#include "test.h"
#include <stdio.h>

int main(int argc, char** argv)
{
    int s = TestClass::someFunction("foobar");
    printf("jadda %d\n", s);
    return 0;
}

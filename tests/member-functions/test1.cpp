#include "test1.h"

static inline int realTest2()
{
    enum { Leet = 1331 };
    return Leet;
}

Test1::Test1()
    : returnValue(41)
{
}

int Test1::test1()
{
    if (returnValue < 42)
        ++returnValue;
    return realTest1();
}

int Test1::test2() const
{
    return realTest2();
}

int Test1::realTest1() const
{
    return returnValue;
}

int Test1::test4()
{
    return FOO;
}

#ifndef TEST2_H
#define TEST2_H

#include "test1.h"

class Test2
{
public:
    Test2();
    Test2(const Test1&, Test1::Type);
};

#endif

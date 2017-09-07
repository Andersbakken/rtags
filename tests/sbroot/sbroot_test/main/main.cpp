#include "../sub1/export/sub1.hpp"
#include <stdlib.h>

int main(void)
{
    int v=SUB1MOD::Csub1::sub1(1); // rc --follow-location sub1( => ../sub1/sub1.cpp:    int Csub1::@sub1(int arg)

    return SUB1MOD::fcn1(v); // rc --follow-location fcn1 => ../sub1/sub1.cpp:    SUB1_EXPORT_FCN int @fcn1(int p1)
}

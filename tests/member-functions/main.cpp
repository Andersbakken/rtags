#include "test1.h"

int main(int argc, char** argv)
{
    Test1 test1;
    int ret = test1.test1();
    ret = test1.test2();
    ret = test1.test3();
    ret = test1.test4();
    return ret;
}

#include "test1.h"
#include "test2.h"

int main(int argc, char** argv)
{
    Test1 hello(argv[0]);
    hello = Test1(argc);
    hello = Test1();
    Test2 world;
    world = Test2(Test1(argc), Test1::Balle);
}

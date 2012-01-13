#include "foo.h"
void bar(); void bar();
void bar();
void bar()
{
}

int main(int argc, char **argv)
{
    if (argc < 3 || argv[1][0] == argv[2][1])
        return 2;

    bar();
    return 0;
}

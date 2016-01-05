#include "include.hpp"
#include "contains_forward_declaration.hpp"

int main()
{
    Local l;
    l.m = new ForwardDecl;
}

struct Opaque;

void foo()
{
    Opaque *op = 0;
}

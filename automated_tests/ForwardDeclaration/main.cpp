#include "include.hpp"
#include "contains_forward_declaration.hpp"

#define FORWARD_DECLARE_STRUCT(__name)     struct Forward ## __name
FORWARD_DECLARE_STRUCT(Decl);

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

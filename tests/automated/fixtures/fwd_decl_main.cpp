#include "fwd_decl_include.hpp"
#include "fwd_decl_contains.hpp"

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

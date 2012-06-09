#include "c.h"

C::C(int v)
    : mem(v)
{

}

C * C::func() const
{
    return new C(mem * 2);
}

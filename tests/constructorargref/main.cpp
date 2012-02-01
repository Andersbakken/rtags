#include "Struct.h"
#include "Class.h"

int main()
{
    Struct s = { 12, 13 };
    Class c(&s);
    return 0;
}

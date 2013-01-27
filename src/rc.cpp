#include "RClient.h"

int main(int argc, char** argv)
{
    RClient rc;
    if (!rc.parse(argc, argv))
        return 1;

    return rc.exec() ? 0 : 1;
}

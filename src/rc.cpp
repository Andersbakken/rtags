#include "RClient.h"

int main(int argc, char** argv)
{
    RClient rc;
    if (!rc.parse(argc, argv))
        return 1;

    rc.exec();
    return 0;
}

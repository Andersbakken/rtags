#include "GRTags.h"

int main(int argc, char **argv)
{
    GRTags grtags;
    return grtags.exec(argc, argv) ? 0 : 1;
}

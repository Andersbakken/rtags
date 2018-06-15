#include "c.h"

int main(int argc, char **argv)
{
    C c(argc);
    delete c.func();

    for (int i=0; i<argc; ++i) {
        C *cc = c.func();
        C *ccc = cc->func();
        delete cc;
        delete ccc;
    }
    return 0;
}

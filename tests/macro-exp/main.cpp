#define FOOBAR ++foo

int main(int argc, char **argv)
{
    int foo = 0;
    foo = 12;
    FOOBAR;
    FOOBAR;
    FOOBAR;
    return 0;
}

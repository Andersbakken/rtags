#define FOOBAR ++foo

int main()
{
    int foo = 0;
    FOOBAR;
    FOOBAR;
    FOOBAR;
    return 0;
}

int func()
{
    int foo;
    ++foo;

    if (foo > foo)
        --foo;
    return foo;
}


int main()
{
    return 0;
}

void foo()
{
    union {
        union {
            int a;
        }
    };

    ++a;
}

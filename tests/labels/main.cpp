int main(int argc, char** argv)
{
    bool ok = true;
    int cnt = 0;
    while (ok) {
        if (++cnt > 10)
            goto foo;
    }
    goto foo;
foo:
    return 0;
}

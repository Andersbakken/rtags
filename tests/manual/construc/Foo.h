#ifndef foo_h
#define foo_h

class Foo
{
public:
    Foo();
    void func();
};

static inline void balle()
{
    Foo f;
    f.func();
}


#endif

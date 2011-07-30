#ifndef test_h
#define test_h

class InlineClass
{
public:
    static inline void inlineFunc(void *ptr)
    {
        int a = 1;
        if (ptr) {
            a = 12;
        }
    }
};

#endif

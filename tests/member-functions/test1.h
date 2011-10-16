#ifndef test1_h
#define test1_h

#define FOO 5

class Test1
{
public:
    Test1();

    int test1();
    int test2() const;
    inline int test3() const { return returnValue; }
    int test4();

private:
    int realTest1() const;

private:
    int returnValue;
};

#endif

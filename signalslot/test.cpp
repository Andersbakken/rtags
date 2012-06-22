#include "signalslot.h"
#include <stdio.h>
#include <string>

class Test
{
public:
    Test()
    {
        signal0.connect(this, &Test::hello0);
        signal1.connect(this, &Test::hello1);
        signal5.connect(this, &Test::hello5);
        something0Signal.connect(this, &Test::something0);
        signal0.connect(something0Signal);
        something1Signal.connect(this, &Test::something1);
        signal1.connect(something1Signal);
    }

    void hello0()
    {
        printf("hello, world\n");
    }

    void something0()
    {
        printf("something\n");
    }

    void something1(int arg)
    {
        printf("something1: %d\n", arg);
    }
    
    void hello1(int arg)
    {
        printf("hello1: %d\n", arg);
    }

    void hello5(int arg1, const std::string& arg2, int arg3, int arg4, int arg5)
    {
        printf("hello5: %d '%s' %d %d %d\n", arg1, arg2.c_str(), arg3, arg4, arg5);
    }

    signalslot::Signal0 signal0;
    signalslot::Signal0 something0Signal;
    signalslot::Signal1<int> signal1;
    signalslot::Signal1<int> something1Signal;
    signalslot::Signal5<int, const std::string&, int, int, int> signal5;
};

int main(int argc, char** argv)
{
    Test test;
    test.signal0();
    test.signal1(5);
    test.signal1(103);
    test.signal1(65530);
    test.signal5(0, "testing 1 2 3", 2, 3, 4);
    test.signal1.disconnect();
    test.signal1(999);
    test.signal0.disconnect(&test, &Test::something0);
    test.signal0();
    test.signal0.disconnect(&test, &Test::hello0);
    test.signal0.disconnect(test.something0Signal);
    test.signal0();

    return 0;
}

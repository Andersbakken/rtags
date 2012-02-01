http://llvm.org/bugs/show_bug.cgi?id=11807

Bug if you see this:

test.cpp:2:7: CXXClass Class
test.cpp:5:5: CXXConstructor Class
test.cpp:8:5: Function main
test.cpp:10:11: Variable t
test.cpp:10:11: ref of CXXConstructor Class test.cpp:5:5
test.cpp:10:5: ref of CXXClass Class test.cpp:2:7
test.cpp:10:11: ref of CXXConstructor Class test.cpp:5:5

instead of this:

test.cpp:2:7: CXXClass Class
test.cpp:5:5: CXXConstructor Class
test.cpp:8:5: Function main
test.cpp:10:11: Variable t
test.cpp:10:11: ref of CXXConstructor Class test.cpp:5:5
test.cpp:10:5: ref of CXXClass Class test.cpp:2:7

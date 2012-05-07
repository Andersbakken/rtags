#!/bin/sh

echo "#ifndef HEADER_H" > Header.h
echo "#define HEADER_H" >> Header.h
echo "#include <stdlib.h>" >> Header.h
echo "#include <stdio.h>" >> Header.h
for j in `seq 1 2`; do
    echo "#include \"Header.h\"" > $j.cpp
    echo "static void foo()" >> $j.cpp
    echo "{" >> $j.cpp
done
for i in `seq 1 10000`; do
    printf "void foobar_$i(int val) {" >> Header.h
    if [ $i -gt 1 ]; then
        printf " foobar_`expr $i - 1`(val); " >> Header.h
    fi
    printf "printf(\"%%d\", $i); }\n" >> Header.h
    for j in `seq 1 100`; do echo "    foobar_$i($j);" >> $j.cpp; done
    echo $i
done
echo "#endif" >> Header.h

for j in `seq 1 2`; do
    echo "}" >> $j.cpp
done
echo "int main() { return 0; }" >> 1.cpp

echo "TEMPLATE = app" > hugetest.pro
echo "SOURCES += 1.cpp 2.cpp" >> hugetest.pro
for i in `seq 3 1000`; do 
    /bin/cp -f 2.cpp $i.cpp
    echo SOURCES += $i.cpp >> hugetest.pro
done

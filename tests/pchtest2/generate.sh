#!/bin/sh

for i in foo1 foo2 foo3 foo4; do 
    echo "#ifndef ${i}_h" > ${i}.h
    echo "#define ${i}_h" >> ${i}.h
    
    for j in `seq 1 4`; do 
        echo "static inline int ${i}_$j(int a, int b, int c) {" >> ${i}.h
        echo "    return a + b + c;" >> ${i}.h
        echo "}" >> ${i}.h
    done
    echo "#endif" >> ${i}.h
done


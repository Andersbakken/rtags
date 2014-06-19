#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/186"

rm -rf $DIR
mkdir -p $DIR && cd $DIR && touch README

cat << "EOF" > test_project.cpp
#include <iostream>

void print_hello();

void print_hello()
{
   std::cout<<"Hello world!"<<std::endl;
}

int main()
{
   print_hello();
   return 0;
}
EOF

rc -v --compile "g++ $DIR/test_project.cpp"

sleep 2
# get cursor info for print_hello() call
echo "################## THIS SHOULD FIND IT ##################"
rc --current-file=$DIR/test_project.cpp -U $DIR/test_project.cpp:12:7
#/home/me/temp/issue186/test_project/test_project.cpp:12:4:           print_hello();
#SymbolName: int print_hello()
#Kind: DeclRefExpr
#Type: FunctionProto
#SymbolLength: 11
#Range: 12:4-12:15
#Targets:
#    /home/me/temp/issue186/test_project/test_project.cpp:5:5:     int print_hello()

# try to simulate an unsaved file (notice the empty line before print_hello() in main)
# get cursor info for print_hello() call at the new location
rc -V $DIR/test_project.cpp
sleep 2
echo "################## THIS SHOULD NOT FIND IT ##################"
rc --current-file=$DIR/test_project.cpp -U $DIR/test_project.cpp:23:7

cat << "EOF" | rc --unsaved-file=$DIR/test_project.cpp:164 -V $DIR/test_project.cpp
#include <iostream>

void print_hello();

void print_hello()
{
   std::cout<<"Hello world!"<<std::endl;
}

int main()
{











   print_hello();
   return 0;
}
EOF

sleep 2
echo "################## THIS SHOULD FIND IT ##################"
rc --current-file=$DIR/test_project.cpp -U $DIR/test_project.cpp:23:7


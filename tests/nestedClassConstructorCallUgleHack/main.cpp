struct A
{
    struct B
    {
        B(int)
        {}
        struct C
        {
            C(int) {}
        };

        void foo()
        {
            C c(12);
            C(12);
            B::C c2(12);
            B::C(12);
            A::B::C c3(12);
            A::B::C(12);

        }

    };
    void foo()
    {
        B::C c(12);
        B::C(12);
        A::B::C c2(12);
        A::B::C(12);
    }
};

int main()
{
    A::B b(13);
    A::B(12);
    A::B::C(13);
    A::B::C c(13);

    return 0;
}


/* The problem is the following. Nested classes' constructors are only invoked
 * on the outermost class. E.g. For: A::B(12) we get the following cursors. Due
 * to ranks in CursorInfo we prefer the target from the callexpr over the target
 * from the typeref but we only have get a symbolLength of 1 since it we can't
 * really know that it refs the constructor with the qualifier A:: first

    A::B::C(13); // 4, 2: CXXFunctionalCastExpr o  /home/abakken/temp/structo/main.cpp,466 (466-477)
    A::B::C(13); // 4, 3: TypeRef r struct A  /home/abakken/temp/structo/main.cpp,466 (466-467) refs StructDecl c A def c:@S@A /home/abakken/temp/structo/main.cpp,7 (0-416)
    A::B::C(13); // 7, 3: TypeRef r struct A::B  /home/abakken/temp/structo/main.cpp,469 (469-470) refs StructDecl c B def c:@S@A@S@B /home/abakken/temp/structo/main.cpp,22 (15-303)
    A::B::C(13); // 10, 3: TypeRef r struct A::B::C  /home/abakken/temp/structo/main.cpp,472 (472-473) refs StructDecl c C def c:@S@A@S@B@S@C /home/abakken/temp/structo/main.cpp,71 (64-114)
    A::B::C(13); // 4, 3: CallExpr r C  /home/abakken/temp/structo/main.cpp,466 (466-477) refs CXXConstructor c C(int) C def c:@S@A@S@B@S@C@F@C#I# /home/abakken/temp/structo/main.cpp,95 (95-104)
    A::B::C(13); // 12, 4: IntegerLiteral o  /home/abakken/temp/structo/main.cpp,474 (474-476)
*/

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
            new C(12);
            B::C c2(12);
            B::C(12);
            new B::C(12);
            A::B::C c3(12);
            A::B::C(12);
            new A::B::C(12);
        }
    };
};

int main()
{
    return 0;
}


/*
  The problem is the following. When invoked on a namespaced or nested class,
  CallExprs to constructors point to the first class or namespace in the chain.
  What we need to do is to replace the symbol with the last TypeRef before the
  CallExpr since this one will have the correct extent.

  B::C c2(12); // 12, 4: DeclStmt o  main.cpp,221 (221-233-233-233)
  B::C c2(12); // 17, 5: VarDecl c c2 def c:main.cpp@221@S@A@S@B@F@foo#@c2 main.cpp,226 (221-232-232-232) C  refs self
  B::C c2(12); // 12, 6: TypeRef r struct A::B  main.cpp,221 (221-222-222-222)  refs StructDecl c B def c:@S@A@S@B main.cpp,22 (15-379-379-379)
  B::C c2(12); // 15, 6: TypeRef r struct A::B::C  main.cpp,224 (224-225-225-225)  refs StructDecl c C def c:@S@A@S@B@S@C main.cpp,71 (64-114-114-114)
  B::C c2(12); // 17, 6: CallExpr r C  main.cpp,226 (226-232-232-232)  refs CXXConstructor c C(int) C def c:@S@A@S@B@S@C@F@C#I# main.cpp,95 (95-104-104-104)
  B::C c2(12); // 20, 7: IntegerLiteral o  main.cpp,229 (229-231-231-231)
  B::C(12); // 12, 4: CXXFunctionalCastExpr o  main.cpp,246 (246-254-254-254)
  B::C(12); // 12, 5: TypeRef r struct A::B  main.cpp,246 (246-247-247-247)  refs StructDecl c B def c:@S@A@S@B main.cpp,22 (15-379-379-379)
  B::C(12); // 15, 5: TypeRef r struct A::B::C  main.cpp,249 (249-250-250-250)  refs StructDecl c C def c:@S@A@S@B@S@C main.cpp,71 (64-114-114-114)
  B::C(12); // 12, 5: CallExpr r C  main.cpp,246 (246-254-254-254)  refs CXXConstructor c C(int) C def c:@S@A@S@B@S@C@F@C#I# main.cpp,95 (95-104-104-104)
  B::C(12); // 17, 6: IntegerLiteral o  main.cpp,251 (251-253-253-253)
  new B::C(12); // 12, 4: CXXNewExpr o  main.cpp,268 (268-280-280-280)
  new B::C(12); // 16, 5: TypeRef r struct A::B  main.cpp,272 (272-273-273-273)  refs StructDecl c B def c:@S@A@S@B main.cpp,22 (15-379-379-379)
  new B::C(12); // 19, 5: TypeRef r struct A::B::C  main.cpp,275 (275-276-276-276)  refs StructDecl c C def c:@S@A@S@B@S@C main.cpp,71 (64-114-114-114)
  new B::C(12); // 16, 5: CallExpr r C  main.cpp,272 (272-280-280-280)  refs CXXConstructor c C(int) C def c:@S@A@S@B@S@C@F@C#I# main.cpp,95 (95-104-104-104)
  new B::C(12); // 21, 6: IntegerLiteral o  main.cpp,277 (277-279-279-279)
*/

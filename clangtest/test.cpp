namespace N
{
void f();
};

using namespace N;
int main()
{
    N::f();
    f();
}

void N::f()
{


}

template<typename T>
void foo(T t)
{
}

template<typename T>
void bar(T t)
{
  foo(t);
}

int main(int argc, char** argv)
{
  bar(1);
  return 0;
}

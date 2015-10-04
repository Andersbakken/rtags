template <typename T>
void foo(T t) {
}

// explicit
template void foo<char>(char t);

// implicit
void some_func() {
    foo<int>(4);
}


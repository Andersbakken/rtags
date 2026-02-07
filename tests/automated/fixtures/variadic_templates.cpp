template <typename... Ts>
struct Tuple {};

template struct Tuple<int>;
template struct Tuple<int, double>;
template struct Tuple<int, double, char>;

template <typename T, typename... Rest>
T first(T val, Rest...) {
    return val;
}

void use_variadic() {
    first(1, 2.0, 'a');
}

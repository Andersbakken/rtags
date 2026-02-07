template <typename T>
T identity(T val) {
    return val;
}

template int identity<int>(int);
template char identity<char>(char);

void use_identity() {
    identity(42);
    identity('a');
}

template <typename T>
struct Container {
    void push(T val) { data_ = val; }
    T pop() { return data_; }
    T data_;
};

template struct Container<int>;
template struct Container<double>;

void use_container() {
    Container<int> ci;
    ci.push(42);
    ci.pop();

    Container<double> cd;
    cd.push(3.14);
}

template <typename T, typename U>
struct Pair {
    T first;
    U second;
};

template struct Pair<int, double>;
template struct Pair<char, char>;

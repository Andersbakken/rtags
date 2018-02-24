struct Chrono
{
    static Chrono now() { return Chrono(); }
};

void bar(int incStartValue) {
    int i = incStartValue;
    auto start = Chrono::now();
}

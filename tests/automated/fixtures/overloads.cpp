struct Printer {
    void print(int x) {}
    void print(double x) {}
    void print(const char *x) {}
};

int add(int a, int b) { return a + b; }
double add(double a, double b) { return a + b; }

void use_overloads() {
    Printer p;
    p.print(1);
    p.print(3.14);
    p.print("hello");

    add(1, 2);
    add(1.0, 2.0);
}

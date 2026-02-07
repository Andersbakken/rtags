class Foo {
public:
    int get() { return 12; }
    void set(int v) { val_ = v; }

private:
    int val_;
    void internal_validate() {}
};

void test_completion() {
    Foo f;
    f.
}

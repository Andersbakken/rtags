namespace outer {

int global_var = 0;

void free_func() {}

namespace inner {

int nested_var = 1;

void nested_func() {
    global_var = 10;
}

class Widget {
public:
    void draw() {}
    static Widget create() { return Widget(); }
};

} // namespace inner

void use_inner() {
    inner::nested_func();
    inner::Widget w;
    w.draw();
    auto w2 = inner::Widget::create();
}

} // namespace outer

void use_outer() {
    outer::free_func();
    outer::inner::nested_func();
    outer::inner::Widget w;
    w.draw();
}

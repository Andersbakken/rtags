struct Base {
    virtual void vfunc() {}
    void non_virtual() {}
};

struct Middle : public Base {
    void vfunc() override {}
    void middle_only() {}
};

struct Derived : public Middle {
    void vfunc() override {}
    void derived_only() {}
};

struct Unrelated {
    void vfunc() {}
};

void use_hierarchy() {
    Derived d;
    d.vfunc();
    d.non_virtual();
    d.middle_only();
    d.derived_only();

    Base *b = &d;
    b->vfunc();
}

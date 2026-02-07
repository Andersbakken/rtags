struct Opaque;

struct Concrete {
    int val;
};

void use_forward_decl() {
    Opaque *op = nullptr;
    Concrete c;
    c.val = 10;
    (void)op;
}

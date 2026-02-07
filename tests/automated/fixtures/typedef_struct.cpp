typedef struct my_struct {
    int a;
    float b;
} my_struct;

struct other_struct {
    int x;
};

typedef other_struct alias_struct;

void use_typedefs() {
    my_struct ms;
    ms.a = 10;
    ms.b = 3.14f;

    alias_struct as;
    as.x = 42;
}

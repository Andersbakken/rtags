#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define CONSTANT 42

int use_macros() {
    int x = CONSTANT;
    int y = MAX(x, 10);
    return y;
}

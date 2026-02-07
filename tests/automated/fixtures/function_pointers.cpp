int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }

typedef int (*BinOp)(int, int);

struct Calculator {
    BinOp operation;
};

void use_fptrs() {
    BinOp op = add;
    int result = op(1, 2);

    Calculator calc;
    calc.operation = sub;
    result = calc.operation(5, 3);
    (void)result;
}

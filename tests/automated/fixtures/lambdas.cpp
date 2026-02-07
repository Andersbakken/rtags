int global_val = 5;

void use_lambdas() {
    int local = 10;

    auto simple = []() { return 1; };
    auto capture_val = [local]() { return local; };
    auto capture_ref = [&local]() { local = 20; };
    auto capture_all = [=]() { return local + global_val; };

    simple();
    capture_val();
    capture_ref();
    capture_all();
}

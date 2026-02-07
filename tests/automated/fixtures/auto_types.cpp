struct Point {
    int x;
    int y;
};

Point make_point(int x, int y) {
    return {x, y};
}

void use_auto() {
    auto p = make_point(1, 2);
    auto &ref = p;
    auto *ptr = &p;
    auto val = p.x + p.y;
}

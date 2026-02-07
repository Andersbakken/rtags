struct Vec2 {
    float x, y;

    Vec2 operator+(const Vec2 &rhs) const {
        return {x + rhs.x, y + rhs.y};
    }

    bool operator==(const Vec2 &rhs) const {
        return x == rhs.x && y == rhs.y;
    }

    Vec2 &operator+=(const Vec2 &rhs) {
        x += rhs.x;
        y += rhs.y;
        return *this;
    }
};

void use_operators() {
    Vec2 a{1.0f, 2.0f};
    Vec2 b{3.0f, 4.0f};
    Vec2 c = a + b;
    bool eq = (a == b);
    a += b;
    (void)c;
    (void)eq;
}

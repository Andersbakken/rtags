enum OldEnum {
    RED,
    GREEN,
    BLUE
};

enum class Color {
    Red,
    Green,
    Blue
};

void use_enums() {
    OldEnum oe = RED;
    Color c = Color::Green;
    (void)oe;
    (void)c;
}

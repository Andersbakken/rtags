class Animal {
public:
    Animal() : legs_(4) {}
    virtual ~Animal() {}
    virtual void speak() {}
    int legs() const { return legs_; }
private:
    int legs_;
};

class Dog : public Animal {
public:
    void speak() override {}
    void fetch() {}
};

class Cat : public Animal {
public:
    void speak() override {}
    void purr() {}
};

void use_animals() {
    Dog d;
    d.speak();
    d.fetch();
    d.legs();

    Cat c;
    c.speak();
    c.purr();
    c.legs();

    Animal *a = &d;
    a->speak();
}

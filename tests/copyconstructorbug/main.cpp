class R
{
public:
    R() {}
    R(const R &r) {}
};

R r;
const R &rr()
{
    return r;
}

int main()
{
    R r = rr();
    return 0;
}

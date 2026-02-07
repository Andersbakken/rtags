struct Config {
    static int max_retries;
    static Config &instance() {
        static Config cfg;
        return cfg;
    }
    void reset() { max_retries = 3; }
};

int Config::max_retries = 5;

void use_static() {
    Config::instance().reset();
    int r = Config::max_retries;
    (void)r;
}

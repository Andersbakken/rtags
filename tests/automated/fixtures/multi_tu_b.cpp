#include "multi_tu_header.hpp"

void caller_b() {
    shared_function();
    Shared s;
    s.method();
    s.value = 10;
}

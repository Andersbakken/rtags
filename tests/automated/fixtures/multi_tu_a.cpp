#include "multi_tu_header.hpp"

void shared_function() {}

void Shared::method() {}

void caller_a() {
    shared_function();
    Shared s;
    s.method();
}

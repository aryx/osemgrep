#include "math_utils.h"
#include <string>

int MathUtils::add(int a, int b) {
    return a + b;
}

double MathUtils::ratio(int a, int b) {
    return static_cast<double>(a) / b;
}

std::string MathUtils::describe(int x) {
    return "value=" + std::to_string(x);
}

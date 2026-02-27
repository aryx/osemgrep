#ifndef MATH_UTILS_H
#define MATH_UTILS_H

#include <string>

// A simple class with methods whose return types require C++ understanding.
class MathUtils {
public:
    // Returns the sum of two ints.
    static int add(int a, int b);

    // Returns a ratio as a double.
    static double ratio(int a, int b);

    // Returns a string description.
    static std::string describe(int x);
};

// A template function — only clangd can resolve the instantiated type.
template <typename T>
T identity(T x) {
    return x;
}

#endif

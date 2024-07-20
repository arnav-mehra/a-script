#include <iostream>

int main() {
    double x = 0;
    for (int a = 0; a < 300000000; a++) {
        x += a;
    }
    std::cout << x;
}
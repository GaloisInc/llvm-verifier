#include <stdlib.h>

void f(int *a) {
    int i;
    a[0] = a[0];
    a[1] = a[1];
}

int main() {
    int *a = malloc(2 * sizeof(int));
    a[0] = 30991;
    a[1] = 3298;
    f(a);
    return a[0] + a[1];
}

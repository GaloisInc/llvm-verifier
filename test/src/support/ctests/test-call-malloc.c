#include <stdlib.h>
#include <sym-api.h>

void f(int *a) {
    int i;
    a[0] = a[0];
    a[1] = a[1];
}

int main() {
    lss_show_mem();
    int *a = malloc(2 * sizeof(int));
    lss_show_mem();
    a[0] = 30991;
    a[1] = 3298;
    f(a);
    return a[0] + a[1];
}

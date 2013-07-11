#include <stdlib.h>

int main() {
    int *foo = malloc(128);
    free(foo);
    return 0;
}

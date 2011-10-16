#include <stdio.h>
#include <sym-api.h>

#define SIZE 10
int main() {
    uint32_t values[SIZE] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    uint32_t *x = lss_fresh_array_uint32(SIZE, 22, values);
    if (x[1] == 1 && x[7] == 7) {
        printf("ok");
        return 0;
    }
    return 1;
}

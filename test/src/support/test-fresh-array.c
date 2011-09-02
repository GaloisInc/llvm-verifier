#include <sym-api.h>

#define SIZE 10
int main() {
    uint32_t *x = fresh_array_uint32(SIZE, 22);
    uint32_t i;
    for(i = 0; i < SIZE; i++) {
        x[i] &= 0x01;
    }
    write_aiger_array_uint32(x, SIZE, "test-fresh-array.aig");
    return 0;
}

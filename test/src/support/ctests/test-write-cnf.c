#include <sym-api.h>

int main() {
    uint32_t x = lss_fresh_uint32(0);
    lss_write_cnf(x + x == 2 * x, "add_mul.cnf");
    return 0;
}

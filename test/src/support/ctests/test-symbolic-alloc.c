#include <stdlib.h>
#include <sym-api.h>

int main() {
    uint32_t size = lss_fresh_uint32(10);
    uint32_t val = lss_fresh_uint32(5);
    uint8_t *buf = malloc(size);
    uint32_t i;
    for(i = 0; i < size && i < 10; i++)
      buf[i] = val;
    return 0;
}

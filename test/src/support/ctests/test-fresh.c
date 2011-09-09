#include <sym-api.h>

int main() {
    uint32_t x = lss_fresh_uint32(22);
    uint32_t y = x & 0x12345678;
    uint8_t inputs[32] = { 0,1,1,0,1,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0 };
    lss_write_aiger_uint32(y, "test-fresh.aig");
    return lss_eval_aiger_uint32(y, inputs, 32);
}

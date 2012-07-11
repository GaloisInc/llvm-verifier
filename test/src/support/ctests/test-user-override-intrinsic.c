

#include <stdio.h>
#include <stdbool.h>
#include <sym-api.h>

int selectb(uint8_t b) 
{
    return b ? 42 : 99;
}

void llvm_memset_p0i8_i64(uint8_t* dst,
                          uint8_t val,
                          uint64_t len,
                          uint32_t align,
                          bool vol
                          )
{
    /* For testing, change input array to const 1 */
    printf("Intrinsic override invoked.\n");
    dst[0] = 1;
    for(int i = 1; i < len; ++i)
        dst[i] = val;
}

int main(int argc, char** argv)
{
    lss_override_llvm_intrinsic("llvm.memset.p0i8.i64", llvm_memset_p0i8_i64);
    uint8_t b     = lss_fresh_uint8(1);
    uint8_t x     = selectb(b);
    uint8_t c0[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    uint8_t e2    = lss_eval_aiger_uint8(x, c0, 8);
    return (e2 == 42); /* expect 42, not 99, because of our memset implementation */
}


#include <stdlib.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.c"

uint64_t Block512_T1(uint64_t h,
                     uint64_t e,
                     uint64_t f,
                     uint64_t g,
                     uint64_t w,
                     uint64_t k)
{
    return h + Sum1 (e) + Ch (e, f, g) + k + w;
}

int main()
{
    uint64_t h = lss_fresh_uint64(0);
    uint64_t e = lss_fresh_uint64(0);    
    uint64_t f = lss_fresh_uint64(0);
    uint64_t g = lss_fresh_uint64(0);
    uint64_t w = lss_fresh_uint64(0);
    uint64_t k = lss_fresh_uint64(0);
    uint64_t r = Block512_T1(h,e,f,g,w,k);
    lss_write_aiger_uint64(r, "impl_AIGs/Block512_T1.aig");    
    return 0;
}

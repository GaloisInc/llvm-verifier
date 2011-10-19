
#include <stdlib.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.c"

uint64_t Block512_T2(uint64_t a,
                     uint64_t b,
                     uint64_t c)
{
    return Sum0 (a) + Maj (a, b, c);
}

int main()
{
    uint64_t a = lss_fresh_uint64(0);
    uint64_t b = lss_fresh_uint64(0);    
    uint64_t c = lss_fresh_uint64(0);
    uint64_t r = Block512_T2(a,b,c);
    lss_write_aiger_uint64(r, "impl_AIGs/Block512_T2.aig");    
    return 0;
}

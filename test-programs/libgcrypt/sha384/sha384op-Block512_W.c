
#include <stdlib.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.c"

uint64_t Block512_W(uint64_t w2,
                    uint64_t w7,
                    uint64_t w15,
                    uint64_t w16)
    
{
    return S1(w2) + w7 + S0(w15) + w16;
}

int main()
{
    uint64_t w2  = lss_fresh_uint64(0);
    uint64_t w7  = lss_fresh_uint64(0);    
    uint64_t w15 = lss_fresh_uint64(0);
    uint64_t w16 = lss_fresh_uint64(0);
    uint64_t r   = Block512_W(w2, w7, w15, w16);
    lss_write_aiger_uint64(r, "impl_AIGs/Block512_W.aig");    
    return 0;
}

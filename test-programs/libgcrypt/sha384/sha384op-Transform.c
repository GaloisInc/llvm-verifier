
#include <stdlib.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.c"

/****************
 * Transform the message W which consists of 16 64-bit-words
 */
static void
Transform (uint64_t *hs, const unsigned char *data)
{
    SHA512_CONTEXT ctx =
        { .h0      = hs[0],
          .h1      = hs[1],
          .h2      = hs[2],
          .h3      = hs[3],
          .h4      = hs[4],
          .h5      = hs[5],
          .h6      = hs[6],
          .h7      = hs[7],
          .nblocks = 0,
          .buf     = {0},
          .count   = 0
        };

    transform(&ctx, data);

    hs[0] = ctx.h0;
    hs[1] = ctx.h1;    
    hs[2] = ctx.h2;
    hs[3] = ctx.h3;
    hs[4] = ctx.h4;
    hs[5] = ctx.h5;
    hs[6] = ctx.h6;
    hs[7] = ctx.h7;
}
                        
int main()
{
    uint64_t* hs  = lss_fresh_array_uint64(8, 0, NULL);
    uint8_t* data = lss_fresh_array_uint8(128, 0, NULL);
    Transform(hs, data);
    lss_write_aiger_array_uint64(hs, 8, "impl_AIGs/Transform.aig");
    return 0;
}

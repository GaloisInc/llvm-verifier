#include <stdlib.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.c"

static const u64 k[] =
  {
    U64_C(0x428a2f98d728ae22), U64_C(0x7137449123ef65cd),
    U64_C(0xb5c0fbcfec4d3b2f), U64_C(0xe9b5dba58189dbbc),
    U64_C(0x3956c25bf348b538), U64_C(0x59f111f1b605d019),
    U64_C(0x923f82a4af194f9b), U64_C(0xab1c5ed5da6d8118),
    U64_C(0xd807aa98a3030242), U64_C(0x12835b0145706fbe),
    U64_C(0x243185be4ee4b28c), U64_C(0x550c7dc3d5ffb4e2),
    U64_C(0x72be5d74f27b896f), U64_C(0x80deb1fe3b1696b1),
    U64_C(0x9bdc06a725c71235), U64_C(0xc19bf174cf692694),
    U64_C(0xe49b69c19ef14ad2), U64_C(0xefbe4786384f25e3),
    U64_C(0x0fc19dc68b8cd5b5), U64_C(0x240ca1cc77ac9c65),
    U64_C(0x2de92c6f592b0275), U64_C(0x4a7484aa6ea6e483),
    U64_C(0x5cb0a9dcbd41fbd4), U64_C(0x76f988da831153b5),
    U64_C(0x983e5152ee66dfab), U64_C(0xa831c66d2db43210),
    U64_C(0xb00327c898fb213f), U64_C(0xbf597fc7beef0ee4),
    U64_C(0xc6e00bf33da88fc2), U64_C(0xd5a79147930aa725),
    U64_C(0x06ca6351e003826f), U64_C(0x142929670a0e6e70),
    U64_C(0x27b70a8546d22ffc), U64_C(0x2e1b21385c26c926),
    U64_C(0x4d2c6dfc5ac42aed), U64_C(0x53380d139d95b3df),
    U64_C(0x650a73548baf63de), U64_C(0x766a0abb3c77b2a8),
    U64_C(0x81c2c92e47edaee6), U64_C(0x92722c851482353b),
    U64_C(0xa2bfe8a14cf10364), U64_C(0xa81a664bbc423001),
    U64_C(0xc24b8b70d0f89791), U64_C(0xc76c51a30654be30),
    U64_C(0xd192e819d6ef5218), U64_C(0xd69906245565a910),
    U64_C(0xf40e35855771202a), U64_C(0x106aa07032bbd1b8),
    U64_C(0x19a4c116b8d2d0c8), U64_C(0x1e376c085141ab53),
    U64_C(0x2748774cdf8eeb99), U64_C(0x34b0bcb5e19b48a8),
    U64_C(0x391c0cb3c5c95a63), U64_C(0x4ed8aa4ae3418acb),
    U64_C(0x5b9cca4f7763e373), U64_C(0x682e6ff3d6b2b8a3),
    U64_C(0x748f82ee5defb2fc), U64_C(0x78a5636f43172f60),
    U64_C(0x84c87814a1f0ab72), U64_C(0x8cc702081a6439ec),
    U64_C(0x90befffa23631e28), U64_C(0xa4506cebde82bde9),
    U64_C(0xbef9a3f7b2c67915), U64_C(0xc67178f2e372532b),
    U64_C(0xca273eceea26619c), U64_C(0xd186b8c721c0c207),
    U64_C(0xeada7dd6cde0eb1e), U64_C(0xf57d4f7fee6ed178),
    U64_C(0x06f067aa72176fba), U64_C(0x0a637dc5a2c898a6),
    U64_C(0x113f9804bef90dae), U64_C(0x1b710b35131c471b),
    U64_C(0x28db77f523047d84), U64_C(0x32caab7b40c72493),
    U64_C(0x3c9ebe0a15c9bebc), U64_C(0x431d67c49c100d4c),
    U64_C(0x4cc5d4becb3e42b6), U64_C(0x597f299cfc657e2a),
    U64_C(0x5fcb6fab3ad6faec), U64_C(0x6c44198c4a475817)
  };

void Block512_Inner(uint64_t* w, uint64_t* outs)
{
    uint64_t h = outs[0];
    uint64_t g = outs[1];
    uint64_t f = outs[2];
    uint64_t e = outs[3];
    uint64_t d = outs[4];
    uint64_t c = outs[5];
    uint64_t b = outs[6];
    uint64_t a = outs[7];
    uint64_t t = outs[8];

    /* begin sha512.c snippet */
    uint64_t t1 = h + Sum1 (e) + Ch (e, f, g) + k[t] + w[t];
    uint64_t t2 = Sum0 (a) + Maj (a, b, c);
    h = g;
    g = f;
    f = e;
    e = d + t1;
    d = c;
    c = b;
    b = a;
    a = t1 + t2;
    t++;
    /* end sha512.c snippet */

    outs[0] = h ;
    outs[1] = g ;
    outs[2] = f ;
    outs[3] = e ;
    outs[4] = d ;
    outs[5] = c ;
    outs[6] = b ;
    outs[7] = a ;
    outs[8] = t ;
}
                        
uint64_t Block512_T1(uint64_t h,
                     uint64_t e,
                     uint64_t f,
                     uint64_t g,
                     uint64_t w,
                     uint64_t k)
{
    return h + Sum1 (e) + Ch (e, f, g) + k + w;
}

uint64_t Block512_T2(uint64_t a,
                     uint64_t b,
                     uint64_t c)
{
    return Sum0 (a) + Maj (a, b, c);
}

uint64_t S0_impl(uint64_t x) {
    return S0(x);
}

uint64_t S1_impl(uint64_t x) {
    return S1(x);
}

uint64_t Block512_W(uint64_t w2,
                    uint64_t w7,
                    uint64_t w15,
                    uint64_t w16)
    
{
    return S1_impl(w2) + w7 + S0_impl(w15) + w16;
}

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

const int MSG_LEN = 127;
                        
int main()
{
    byte cxt[512] = {0};
    byte *res;
    byte *text = lss_fresh_array_uint8(MSG_LEN, 0, NULL);

    sha384.init(cxt);
    sha384.write(cxt, text, MSG_LEN);
    sha384.final(cxt);
    res = sha384.read(cxt);

    lss_write_aiger_array_uint8(res, 48, "impl_AIGs/sha384_top.aig");
    return 0;
}

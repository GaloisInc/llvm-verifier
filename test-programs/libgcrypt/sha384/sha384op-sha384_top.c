
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.c"

#define MSG_LEN (1)

uint64_t llvm_objectsize_i64(uint8_t* obj,  bool maxOrMin) 
{
    return 128;
}

void* __memset_chk (void* dest, int val, size_t len, size_t slen)
{
    for(uint32_t i = 0; i < len; ++i)
        ((uint8_t*)dest)[i] = val;
    return dest;
}

int main()
{
    byte cxt[512] = {0};
    byte *res;
    byte *text = lss_fresh_array_uint8(MSG_LEN, 0, NULL);

    lss_override_llvm_intrinsic("llvm.objectsize.i64", llvm_objectsize_i64);

    sha384.init(cxt);
    sha384.write(cxt, text, MSG_LEN);
    sha384.final(cxt);
    res = sha384.read(cxt);

    lss_write_aiger_array_uint8(res, 48, "impl_AIGs/sha384_top.aig");
    return 0;
}

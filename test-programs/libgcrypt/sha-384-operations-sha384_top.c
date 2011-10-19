
#include <stdlib.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.c"

int main()
{
    byte cxt[512] = {0};
    byte *res;
    byte *text = lss_fresh_array_uint8(1, 0, NULL);

    sha384.init(cxt);
    sha384.write(cxt, text, 1);
    sha384.final(cxt);

    res = sha384.read(cxt);

    lss_write_aiger_array_uint8(res, 48, "gcrypt_impl_AIGs/sha384_top.aig");
    return 0;
}

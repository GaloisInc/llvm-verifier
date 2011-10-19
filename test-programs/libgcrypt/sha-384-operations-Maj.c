#include "sha512.c"
#include "sym-api.h"

int main(int argc, char** argv) 
{
    uint64_t x = lss_fresh_uint64(0);
    uint64_t y = lss_fresh_uint64(0);
    uint64_t z = lss_fresh_uint64(0);

    uint64_t r = Maj(x, y, z);
    lss_write_aiger_uint64(r, "gcrypt_impl_AIGs/Maj.aig");
}

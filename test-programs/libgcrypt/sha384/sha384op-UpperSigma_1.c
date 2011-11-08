#include "sha512.c"
#include "sym-api.h"

int main(int argc, char** argv) 
{
    uint64_t x = lss_fresh_uint64(0);

    uint64_t r = Sum1(x);
    lss_write_aiger_uint64(r, "impl_AIGs/UpperSigma_1.aig");
}

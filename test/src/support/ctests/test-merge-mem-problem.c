#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int f(uint32_t x)
{
    if (x == 5)
        return 1;
    printf ("The presence of this call results in a merge memory failure\n");
    return 1;
}

int main(int argc, char **argv)
{
    uint32_t x = lss_fresh_uint32(1);
    int r = f(x);
    printf("Program result: %d\n", r);
    return r;
}

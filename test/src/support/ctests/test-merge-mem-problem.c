#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int f(uint32_t x)
{
    if (x == 5)
        return 1;
    printf ("If we do not pre-initialize global data properly, the use of this string will result in a merge memory failure\n");
    return 1;
}

int main(int argc, char **argv)
{
    uint32_t x = lss_fresh_uint32(1);
    return f(x) == 1;
}

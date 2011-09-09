#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int e1(int b)
{
    if (b) {
        lss_abort("dead path 1");
        return 1; // unreachable
    }
    else {
        lss_abort("dead path 2");
        return 0; // unreachable
    }
}

int main(int argc, char **argv)
{
    int b = lss_fresh_uint8(1);
    return e1(b);
}

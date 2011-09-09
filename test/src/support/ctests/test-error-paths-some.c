#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int e1(int b)
{
    if (b) {
        return 1;
    }
    else {
        lss_abort("dead path!");
        return 0; // unreachable
    }
}

int main(int argc, char **argv)
{
    int b = lss_fresh_uint8(1);
    int r = e1(b);
    printf("Program result: %d\n", r);
    return 0;
}

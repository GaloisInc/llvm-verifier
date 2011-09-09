#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int e1(uint8_t b0, uint8_t b1)
{
    if (b0) {
        if (b1) {
            return 3;
        }
        else {
            lss_abort("b0 & !b1");
            // unreachable
            return 2;
        }
    }
    else {
        if (b1) {
            lss_abort("!b0 & b1");
            // unreachable
            return 1;
        }
        else {
            return 0;
        }
    }
}

int main(int argc, char **argv)
{
    int b0 = fresh_uint8(1);
    int b1 = fresh_uint8(1);
    int r = e1(b0, b1);
    printf("Program result: %d\n", r);
    return 0;
}

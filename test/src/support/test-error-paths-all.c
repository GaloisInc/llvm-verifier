#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

uint8_t fresh_uint8(uint8_t);
void lss_abort(char* msg);

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
    int b = fresh_uint8(1);
    return e1(b);
}

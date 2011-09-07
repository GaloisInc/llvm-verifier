#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

uint8_t fresh_uint8(uint8_t);
void lss_abort(char* msg);

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
    int b = fresh_uint8(1);
    int r = e1(b);
    printf("Program result: %d\n", r);
    return 0;
}

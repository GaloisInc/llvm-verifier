#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int f(uint32_t x)
{
    if (x == 5) {
        if (x != 5) {
            printf("failure to cull infeasible path\n");
            lss_show_path();
            return 0;
        }
    }
    return 1;
}

int main(int argc, char **argv)
{
    uint32_t x = lss_fresh_uint32(1);
    int r = f(x);
    printf("Program result: %d\n", r);
    return r;
}

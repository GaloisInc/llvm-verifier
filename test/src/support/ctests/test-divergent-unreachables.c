#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int main(int argc, char** argv) 
{
    uint8_t x = lss_fresh_uint8(0);

    if (x > 5) {
    } else {
        /* if x < 5, then the assertion failure should trigger. */
        assert (x > 5); 
    }
    return 1;
}

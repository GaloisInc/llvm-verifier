#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

void blah();

int main(int argc, char** argv) 
{
    uint8_t x = lss_fresh_uint8(0);

    if (x > 5) {
    }
    else {
        blah();
    }
    return 1;
}

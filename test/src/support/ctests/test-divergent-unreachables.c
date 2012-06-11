#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <sym-api.h>

int main(int argc, char** argv) 
{
    uint8_t x = lss_fresh_uint8(0);

    if (x > 5) {
    }
    else {
      /* TODO: Figure out why we get errors here. */
        /* if x > 5, then the assertion passes but the rest of the path */
        /* is infeasible, and should be reported as such by LSS. */
        /* if x < 5, then the assertion failure should trigger. */
        /* Thus, LSS should report two path errors for this program. */
        assert (x > 5); 
        /* unreachable */
    }
    return 1;
}

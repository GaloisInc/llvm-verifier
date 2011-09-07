

#include <stdio.h>
#include <sym-api.h>

int select(uint8_t b) 
{
    return b ? 42 : 99;
}

int main(int argc, char** argv)
{
    uint8_t b  = fresh_uint8(1);
    uint8_t x  = select(b);
    
    uint8_t c0[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    uint8_t c1[8] = { 1, 0, 0, 0, 0, 0, 0, 0 };
    
    uint8_t e1 = eval_aiger_uint8(x, c0, 8);
    uint8_t e2 = eval_aiger_uint8(x, c1, 8);
    
    printf("e1=%d, e2=%d\n", e1, e2);
    return 0;
}

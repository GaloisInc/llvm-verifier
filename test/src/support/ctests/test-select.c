

#include <stdio.h>
#include <sym-api.h>

int select(uint8_t b) 
{
    return b ? 42 : 99;
}

int main(int argc, char** argv)
{
    uint8_t b  = lss_fresh_uint8(1);
    uint8_t x  = select(b);
    
    uint8_t c0[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    uint8_t c1[8] = { 1, 0, 0, 0, 0, 0, 0, 0 };
    
    uint8_t e1 = lss_eval_aiger_uint8(x, c1, 8);
    uint8_t e2 = lss_eval_aiger_uint8(x, c0, 8);
    
    /* Run lss with --dbug=2 to see return value */
    return (e1 == 42 && e2 == 99);
}

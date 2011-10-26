#include <sym-api.h>

int main()
{
    uint32_t x = lss_fresh_uint32(0);
    uint32_t y = lss_fresh_uint32(0);
    uint32_t z = lss_fresh_uint32(0);

    uint32_t r1 = x + 42;
    lss_aiger_add_output_uint32(r1);
    uint32_t r2 = y + 99;
    lss_aiger_add_output_uint32(r2);

    lss_write_aiger("test-fresh-incremental-1.aig");

    uint32_t r3 = x + y + z + 4299;
    lss_aiger_add_output_uint32(r3);
    lss_write_aiger("test-fresh-incremental-2.aig");

    /* NB: We're only doing very lightweight testing here, as we have not
       written an arbitrary-width AIG evaluation routine.  However, we've done
       the following by hand to sanity check:

       extern AIG f("test-fresh-incremental-1.aig") : ([32],[32],[32]) -> ([32],[32]);
       extern AIG g("test-fresh-incremental-2.aig") : ([32],[32],[32]) -> [32];
       
       f' : ([32],[32],[32]) -> ([32],[32]);
       f' (x,y,z) = (x+42,y+99);
       
       g' : ([32],[32],[32]) -> [32];
       g' (x,y,z) = x+y+z+4299;
       
       theorem one : {x y z}. f(x,y,z) == f'(x,y,z);
       theorem two : {x y z}. g(x,y,z) == g'(x,y,z);
    */

    return 0;
}


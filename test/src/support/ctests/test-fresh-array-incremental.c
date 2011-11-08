#include <sym-api.h>

#define SIZE 10
int main() {
    uint64_t* arr = lss_fresh_array_uint64(SIZE, 0, NULL);
    uint32_t x    = lss_fresh_uint32(0);
    for(uint32_t i = 0; i < SIZE; i++)
        arr[i] += 1;
    lss_aiger_add_output_array_uint64(arr, SIZE);
    lss_aiger_add_output_uint32(x + 99);
    lss_write_aiger("test-fresh-array-incremental-1.aig");
    uint8_t y = lss_fresh_uint8(0);
    lss_aiger_add_output_uint8(y + 42);
    lss_write_aiger("test-fresh-array-incremental-2.aig");

    /* NB: We're only doing very lightweight testing here, as we have not
       written an arbitrary-width AIG evaluation routine.  However, we've done
       the following by hand to sanity check:

       extern AIG f("test-fresh-array-incremental-1.aig") : ([10][64],[32]) -> ([10][64],[32]);
       extern AIG g("test-fresh-array-incremental-2.aig") : ([10][64],[32],[8]) -> [8];
       
       f' : ([10][64],[32]) -> ([10][64],[32]);
       f' (arr,x) = ([| v + 1 || v <- arr |], x + 99);
       
       g' : ([10][64],[32],[8]) -> [8];
       g' (arr,x,y) = y + 42;
       
       theorem one : {arr x}. f(arr,x) == f'(arr,x);
       theorem two : {arr x y}. g(arr,x,y) == g'(arr,x,y);
    */

    return 0;
}

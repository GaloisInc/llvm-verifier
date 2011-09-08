#include <stdio.h>
#include <sym-api.h>

/* Tests that malloc() with a symbolic length when it is not supported
   by the memory model correctly kills a path. */

int* foo(int b, int sz) 
{
    int* p;
    if (b)
        p = (int*) malloc(sizeof(int));
    else
        p = (int*) malloc(sz);
    *p = 99;
    return p;
}

int main(int argc, char** argv) 
{
    uint32_t symsz = fresh_uint32(1);
    uint8_t b      = fresh_uint8(1);
    int* p = foo(b, symsz);
    return 99 == *p;
    /* Assuming that a memory model that does not support symbolic
       lengths is used, lss should report that one path yielded errors
       (the path in foo() in which a symbolic malloc size was used), and
       main() should return a concrete result of 1. */
}

/* TODO: check :

   concrete indcies into a symbolic length array
   symbolic indices into a concrete length array
   symbolic indices into a symbolic length array

   Ensure warnings and non-crashiness.
*/

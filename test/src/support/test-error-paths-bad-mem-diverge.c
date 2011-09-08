#include <stdio.h>
#include <sym-api.h>

/* Tests invalid memory access only on one of two divergent paths */

int foo(int b, int* p) 
{
    if (b)
        return *(p+100) + 57; // obviously bad
    else 
        return *p + 57;
}

int main(int argc, char** argv) 
{
    uint8_t b = fresh_uint8(1);
    int x = 42;
    return 99 == foo(b, &x);
    /* lss should report that one path yielded errors, and main() should
       return a concrete result of 1.*/
}


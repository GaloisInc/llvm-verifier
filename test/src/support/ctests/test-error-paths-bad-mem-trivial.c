#include <stdio.h>
#include <sym-api.h>

int main(int argc, char** argv) 
{
    int x  = 42;
    int* p = &x;
    return *(p+100) + 57;
    /* lss should report that all paths yielded errors */
}


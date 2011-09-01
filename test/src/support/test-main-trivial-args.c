#include <stdio.h>

int main(int argc, char** argv) 
{
    printf("Hello, world.");
    for (int i = 0; i < argc; ++i)
        printf("argv[%d]: %s", i, argv[i]);
    return 0;
}


#include <stdlib.h>
#include <stdio.h>

#include "sha512.h"

char text[] = "foo";

int main()
{
    byte cxt[512] = {0};
    byte *res;

    sha384.init(cxt);
    sha384.write(cxt, text, 3);
    sha384.final(cxt);
        
    res = sha384.read(cxt);

    for(int i=0; i<47; ++i) {
        printf("%02x:", res[i]);
    }
    printf("%x\n", res[47]);

    return 0;
}

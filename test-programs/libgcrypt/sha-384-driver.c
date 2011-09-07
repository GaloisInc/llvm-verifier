
#include <stdlib.h>
#include <stdio.h>

#include "sha512.h"

/* To avoid that a compiler optimizes certain memset calls away, these
 *    macros may be used instead. */
#define wipememory2(_ptr,_set,_len) do { \
	              volatile char *_vptr=(volatile char *)(_ptr); \
	              size_t _vlen=(_len); \
	              while(_vlen) { *_vptr=(_set); _vptr++; _vlen--; } \
	                  } while(0)
#define wipememory(_ptr,_len) wipememory2(_ptr,0,_len)

void
_gcry_burn_stack (int bytes)
{
    char buf[64];

    wipememory (buf, sizeof buf);
    bytes -= sizeof buf;
    if (bytes > 0)
        _gcry_burn_stack (bytes);
}

char text[] = "Hello, world";

int main() {

	byte cxt[512] = {0};
	byte *res;

	sha384.init(cxt);
	sha384.write(cxt, text, 12);
	sha384.final(cxt);

	res = sha384.read(cxt);

	for(int i=0; i<47; ++i) {
		printf("%02x:", res[i]);
	}
	printf("%x\n", res[47]);

	return 0;
}

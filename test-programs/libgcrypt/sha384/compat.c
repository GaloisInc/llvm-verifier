
#include <stdlib.h>

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


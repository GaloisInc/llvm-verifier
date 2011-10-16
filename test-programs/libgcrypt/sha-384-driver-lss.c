
#include <stdlib.h>
#include <stdio.h>
#include <sym-api.h>

#include "sha512.h"

void sha384_init (void *context);
void sha512_write (void *context, const void *inbuf_arg, size_t inlen);
byte * sha512_read (void *context);
void sha512_final (void *context);

int main() {

	byte cxt[512] = {0};
	byte *res;
	byte *text = lss_fresh_array_uint8(4, 0, NULL);
	text[3] = '\0';
	byte res2[48];
	byte input[32] = {0};

	sha384_init(cxt);
	sha512_write(cxt, text, 3);
	sha512_final(cxt);

	res = sha512_read(cxt);

	lss_write_aiger_array_uint8(res, 48, "sha384.aig");
	lss_eval_aiger_array_uint8(res, res2, 48, input, 32);
	for(int i=0; i<47; ++i) {
		printf("%02x:", res2[i]);
	}
	printf("%x\n", res2[47]);

	return 0;
}

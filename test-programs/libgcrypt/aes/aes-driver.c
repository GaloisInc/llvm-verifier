
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sym-api.h>
#include "aes.h"

/* NB: Added for LSS on Mac OS X */
void* __memcpy_chk (void* dst, void* src, size_t n, size_t slen)
{
    for(uint32_t i = 0; i < n; ++i)
        ((uint8_t*)dst)[i] = ((uint8_t*)src)[i];
    return dst;
}

void hexdump(byte *, int);

int encrypt(void *cxt, const char *pt, int len, char *ct) {
	byte block[16];
	int l;

	for(int i=0; i<len; i+=16) {
		if(i + 16 < len) {
			l = 16;
		} else {
			l = ((len - 16) % 16);
		}

		bzero(block, sizeof(block));
		memcpy(block, pt+i, l);
		aes.encrypt(cxt, ct+i, block);
	}

	return 1;
}

int decrypt(void *cxt, const char *ct, int len, char *pt) {
	byte block[16];

	for(int i=0; i<len; i+=16) {
		bzero(block,sizeof(block));
		memcpy(block, ct+i, sizeof(block));
		aes.decrypt(cxt, pt+i, block);
	}

	return 1;
}

void hexdump(byte *bs, int len) {
	if(len < 1) {
		return;
	}

	printf("%02x", bs[0]);
	for(int i = 1; i < len; ++i) {
		if(i%16 == 0) {
			printf("\n");
		} else if(i % 4 == 0) {
			printf("  ");
		} else {
			printf(" ");
		}
		printf("%02x", bs[i]);
	}
	printf("\n");
}

int main() {
	char cxt[512] = {0};
	byte key[] = "hello, worldxxyy";
	byte pt[] = "this is the plaintext";
	byte ct[32] = {0};
	byte pt2[32] = {0};

	aes.set_key(cxt, key, 16);
	encrypt(cxt, pt, sizeof(pt), ct);
	printf("cipher text:\n");
	hexdump(ct, 32);

	decrypt(cxt, ct, 32, pt2);
	printf("decrypted text:\n");
	hexdump(pt2, 32);
	printf("%s\n", pt2);

	return 0;
}

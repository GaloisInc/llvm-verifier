#ifndef __SHA_512
#define __SHA_512

#include <config.h>
#include "bithelp.h"
#include "cipher.h"
#include "hash-common.h"

typedef void(*HASH_INIT)(void *);
typedef void(*HASH_WRITE)(void *, const void *, size_t);
typedef void(*HASH_FINAL)(void *);
typedef byte *(*HASH_READ)(void *);

struct hash {
	HASH_INIT  init;
	HASH_WRITE write;
	HASH_FINAL final;
	HASH_READ  read;
	size_t     size;
};

extern struct hash sha384;
extern struct hash sha512;

#endif

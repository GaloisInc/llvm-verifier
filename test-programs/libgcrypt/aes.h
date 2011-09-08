#ifndef __AES_H
#define __AES_H

#include <config.h>
#include "bithelp.h"
#include "cipher.h"
#include "hash-common.h"

struct aes {
	int (*set_key)(void *, const byte *, const unsigned);
	void (*encrypt)(void *, byte *, const byte *);
	void (*decrypt)(void *, byte *, const byte *);
	int size;
};

extern struct aes aes;

#endif


.PHONY: all
all:

include ../libgcrypt.mk
LSS := ../../cabal-dev/bin/lss
CC := clang
CFLAGS := $(libgcrypt_flags) -I$(extra_include) -Wall -I../../../sym-api -O0

.PHONY: clean
clean: $(clean-targets)

%.bc: %.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<

sym-api.bc: ../../../sym-api/sym-api.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<

.PHONY: distclean
distclean: clean
	$(RM) -r $(libgcrypt)
	$(RM) $(libgcrypt_pkg)

all: aes-test
aes-test_objects := aes-driver.bc compat.bc rijndael.bc
aes-test: $(libgcrypt)/config.h $(aes-test_objects)
	llvm-ld -o $@ $(aes-test_objects)

clean: clean-aes-test
clean-aes-test:
	$(RM) $(aes-test_objects) aes-test.bc aes-test

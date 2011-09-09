
libgcrypt     := libgcrypt-1.5.0
libgcrypt_pkg := $(libgcrypt).tar.bz2
libgcrypt_url ?= ftp://ftp.gnupg.org/gcrypt/libgcrypt/$(libgcrypt_pkg)

LSS := ../../cabal-dev/bin/lss
CC := clang
CFLAGS := -I$(libgcrypt) -I$(libgcrypt)/src -I$(libgcrypt)/cipher -Wall \
          -I../../sym-api -O0

.PHONY: all
all:

.PHONY: clean
clean: $(clean-targets)


%.bc: %.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<

sym-api.bc: ../../sym-api/sym-api.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<


$(libgcrypt_pkg):
	curl $(libgcrypt_url) > $@

$(libgcrypt)/.token: $(libgcrypt_pkg)
	tar -jxvf $<
	touch $@

$(libgcrypt)/config.h: $(libgcrypt)/.token
	cd $(libgcrypt); ./configure --enable-ciphers=aes \
		--enable-digests=sha512 --enable-pubkey-ciphers=rsa \
		--disable-aesni-support --disable-padlock-support

.PHONY: distclean
distclean: clean
	$(RM) -r $(libgcrypt)
	$(RM) $(libgcrypt_pkg)


all: sha384-test sha384-test-lss
sha384-test_objects := sha512.bc sha-384-driver.bc compat.bc
sha384-test-lss_objects := sha512.bc sha-384-driver-lss.bc compat.bc \
                           sym-api.bc
sha384-test: $(libgcrypt)/config.h $(sha384-test_objects)
	llvm-ld -disable-opt -o $@ $(sha384-test_objects)
sha384-test-lss: $(libgcrypt)/config.h $(sha384-test-lss_objects)
	llvm-ld -disable-opt -o $@ $(sha384-test-lss_objects)

clean: clean-sha384-test
clean-sha384-test:
	$(RM) $(sha384-test_objects) $(sha384-test-lss_objects) \
        sha384-test.bc sha384-test sha384-test-lss.bc sha384-test-lss

sha384.aig: sha384-test-lss
	${LSS} sha384-test-lss.bc

all: aes-test
aes-test_objects := aes-driver.bc compat.bc rijndael.bc
aes-test: $(libgcrypt)/config.h $(aes-test_objects)
	llvm-ld -o $@ $(aes-test_objects)

clean: clean-aes-test
clean-aes-test:
	$(RM) $(aes-test_objects) aes-test.bc aes-test

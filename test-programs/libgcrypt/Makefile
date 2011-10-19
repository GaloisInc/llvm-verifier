
libgcrypt     := libgcrypt-1.5.0
libgcrypt_pkg := $(libgcrypt).tar.bz2
libgcrypt_url ?= ftp://ftp.gnupg.org/gcrypt/libgcrypt/$(libgcrypt_pkg)
extra_include := /opt/local/include

LSS := ../../cabal-dev/bin/lss
CC := clang
CFLAGS := -I$(libgcrypt) -I$(libgcrypt)/src -I$(libgcrypt)/cipher -I$(extra_include) -Wall -I../../sym-api -O0

# Please don't delete temporary bitcode files
.PRECIOUS: %.bc
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

all: sha384-test aigs
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

# NB: Keep sha38_top at the end of this list.  Seriously.
FUNCTIONS=Ch Maj UpperSigma_0 UpperSigma_1 LowerSigma_0 LowerSigma_1 sha384_top 
IMPL_AIGS=$(patsubst %,gcrypt_impl_AIGs/%.aig,$(FUNCTIONS))
SPEC_AIGS=$(patsubst %,spec_AIGs/%.aig,$(FUNCTIONS))

aigs: $(IMPL_AIGS) $(SPEC_AIGS)

sha-384-operations-sha384_top.bc: sha512.c sha-384-operations-sha384_top.c compat.bc sym-api.bc 
	$(CC) $(CFLAGS) -emit-llvm -c -o sha-384-operations-sha384_top_partial.bc sha-384-operations-sha384_top.c
	llvm-link -o $@ sha-384-operations-sha384_top_partial.bc compat.bc sym-api.bc

gcrypt_impl_AIGs/%.aig: sha-384-operations-%.bc
	${LSS} $<

spec_AIGs/sha384_top.aig: shaProofs.cry
	cryptol -n shaProofs.cry -c ":s sbv" -c ":set sbv_targetLanguage=AIG" -c ":set sbv_aigVCMode=Ignore" -c ":trans sha384_top \"spec_AIGs/sha384_top.aig\""

spec_AIGs/%.aig: SHA384.cry
	cryptol SHA384.cry -c ":s sym" -c ":fm $* \"spec_AIGs/$*.aig\""

clean: clean-aigs 

clean-aigs:
	rm -f gcrypt_impl_AIGs/*.aig spec_AIGs/*.aig


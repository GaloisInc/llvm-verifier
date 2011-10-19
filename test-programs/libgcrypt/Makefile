# Orchestrates the libgcrypt SHA384 equivalence checking proof against
# the Cryptol reference implementation.

all: aigs
prove: aigs
	time abc -c "cec -v gcrypt_impl_AIGs/sha384_top.aig spec_AIGs/sha384_top.aig"

include libgcrypt.mk
LSS    := ../../cabal-dev/bin/lss
CC     := clang
CFLAGS := $(libgcrypt_flags) -Wall -I../../sym-api -O0

### AIG rules

FUNCTIONS :=   \
  Ch           \
  Maj          \
  UpperSigma_0 \
  UpperSigma_1 \
  LowerSigma_0 \
  LowerSigma_1 \
  sha384_top   # NB: Keep sha384_top at the end of this list.

IMPL_AIGS := $(patsubst %,gcrypt_impl_AIGs/%.aig,$(FUNCTIONS))
SPEC_AIGS := $(patsubst %,spec_AIGs/%.aig,$(FUNCTIONS))

aigs: $(IMPL_AIGS) $(SPEC_AIGS)

gcrypt_impl_AIGs/%.aig: sha-384-operations-%.bc
	${LSS} $<

spec_AIGs/sha384_top.aig: shaProofs.cry
	cryptol -n shaProofs.cry -c ":s sbv" -c ":set sbv_targetLanguage=AIG" -c ":set sbv_aigVCMode=Ignore" -c ":trans sha384_top \"spec_AIGs/sha384_top.aig\""

spec_AIGs/%.aig: SHA384.cry
	cryptol SHA384.cry -c ":s sym" -c ":fm $* \"spec_AIGs/$*.aig\""

### Bitcode rules

sha-384-operations-sha384_top.bc: sha512.c sha-384-operations-sha384_top.c compat.bc sym-api.bc 
	$(CC) $(CFLAGS) -emit-llvm -c -o sha-384-operations-sha384_top_partial.bc \
          sha-384-operations-sha384_top.c
	llvm-link -o $@ sha-384-operations-sha384_top_partial.bc compat.bc sym-api.bc

sym-api.bc: ../../sym-api/sym-api.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<

%.bc: %.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $< 


### Other targets

clean: 
	$(RM) gcrypt_impl_AIGs/*.aig spec_AIGs/*.aig *.bc
distclean: clean
	$(RM) -r $(libgcrypt)
	$(RM) $(libgcrypt_pkg)

.PRECIOUS: %.bc
.PHONY: all aigs clean distclean prove

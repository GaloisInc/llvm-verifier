# Orchestrates the equivalence checking proof of libgcrypt SHA384
# against the Cryptol reference implementation (SHA384.cry).

all: aigs
prove: aigs
	time cryptol -n shaProofs.cry -c ':s sbv_abc' -c ':prove' 

TOP := ../../..
include ../libgcrypt.mk
# LSS    := $(TOP)/cabal-dev/bin/lss --errpaths
LSS    := lss --errpaths -b saw -d 4
CC     := clang
CFLAGS := $(libgcrypt_flags) -Wall -I$(TOP)/sym-api -O0

### AIG rules

FUNCTIONS :=     \
  Ch             \
  Maj            \
  UpperSigma_0   \
  UpperSigma_1   \
  LowerSigma_0   \
  LowerSigma_1   \
  Block512_T1    \
  Block512_T2    \
  Block512_W     \
  Block512_Inner \
  Transform      \
  sha384_top   # NB: Keep sha384_top at the end of this list.

IMPL_AIGS := $(patsubst %,impl_AIGs/%.aig,$(FUNCTIONS))

aigs: $(libgcrypt)/config.h prep $(IMPL_AIGS) 
prep:
	@mkdir -p impl_AIGs

impl_AIGs/%.aig: sha384op-%.bc
	@echo Generating implementation AIG $*.aig via LSS...
	@mkdir -p impl_AIGs
	@${LSS} $<

### LLVM bitcode rules

sha384op-sha384_top.bc: sha384op-sha384_top.c sha512.c compat.bc sym-api.bc 
	@echo Building libgcrypt SHA384 driver...
	@$(CC) $(CFLAGS) -emit-llvm -c -o tmp.bc sha384op-sha384_top.c
	@llvm-link -o $@ tmp.bc compat.bc sym-api.bc

sym-api.bc: $(TOP)/sym-api/sym-api.c
	@echo Generating sym-api.bc...
	@$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<

%.bc: %.c
	@echo Generating $@...
	@$(CC) $(CFLAGS) -emit-llvm -c -o $@ $< 

### Other targets

clean: 
	$(RM) -r *.bc impl_AIGs
distclean: clean
	$(RM) -r $(libgcrypt)
	$(RM) $(libgcrypt_pkg)

.PRECIOUS: %.bc
.PHONY: all aigs clean distclean prep prove

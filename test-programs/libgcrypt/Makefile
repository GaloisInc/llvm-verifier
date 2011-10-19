# Orchestrates the equivalence checking proof of libgcrypt SHA384
# against the Cryptol reference implementation (SHA384.cry).

all: aigs
just-abc: aigs
	time abc -c "cec -v impl_AIGs/sha384_top.aig spec_AIGs/sha384_top.aig"
prove: aigs
	time abc -c "cec -v impl_AIGs/sha384_top.aig spec_AIGs/sha384_top.aig"
	time cryptol -n shaProofs.cry -c ":s sbv_abc" -c ":prove"

include libgcrypt.mk
LSS    := ../../cabal-dev/bin/lss --dbug=0 --errpaths
CC     := clang
CFLAGS := $(libgcrypt_flags) -Wall -I../../sym-api -O0

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
# NB: Keep spec_AIGs/block512.aig at the start of this list
SPEC_AIGS := spec_AIGs/block512.aig $(patsubst %,spec_AIGs/%.aig,$(FUNCTIONS)) 

aigs: prep $(IMPL_AIGS) $(SPEC_AIGS)
prep: 
	@mkdir -p impl_AIGs spec_AIGs

impl_AIGs/%.aig: sha384op-%.bc
	@echo Generating implementation AIG $*.aig via LSS...
	@${LSS} $<

spec_AIGs/block512.aig: SHA384-Ops.cry SHA384.cry
	@echo Generating implementation AIG block512.aig specification AIG via Cryptol...
	@cryptol -n SHA384-Ops.cry                             \
           -c ":s sbv"                                         \
           -c ":set sbv_targetLanguage=AIG"                    \
           -c ":set sbv_aigVCMode=Ignore"                      \
           -c ":trans block512 \"spec_AIGs/block512.aig\""

spec_AIGs/sha384_top.aig: shaProofs.cry SHA384-Ops.cry SHA384.cry 
	@echo Generating toplevel grafted specification AIG via Cryptol...
	@cryptol -n shaProofs.cry                              \
           -c ":s sbv"                                         \
           -c ":set sbv_targetLanguage=AIG"                    \
           -c ":set sbv_aigVCMode=Ignore"                      \
           -c ":trans sha384_top \"spec_AIGs/sha384_top.aig\""

spec_AIGs/%.aig: SHA384-Ops.cry SHA384.cry
	@echo Generating specification AIG $*.aig via Cryptol...
	@cryptol -q SHA384-Ops.cry		\
           -c ":s sbv"				\
           -c ":set sbv_targetLanguage=AIG"	\
           -c ":set sbv_aigVCMode=Ignore"       \
           -c ":trans $* \"spec_AIGs/$*.aig\""

### LLVM bitcode rules

sha384op-sha384_top.bc: sha384op-sha384_top.c sha512.c compat.bc sym-api.bc 
	@echo Building libgcrypt SHA384 driver...
	@$(CC) $(CFLAGS) -emit-llvm -c -o tmp.bc sha384op-sha384_top.c
	@llvm-link -o $@ tmp.bc compat.bc sym-api.bc

sym-api.bc: ../../sym-api/sym-api.c
	@echo Generating sym-api.bc...
	@$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<

%.bc: %.c
	@echo Generating $@...
	@$(CC) $(CFLAGS) -emit-llvm -c -o $@ $< 

### Other targets

clean: 
	$(RM) -r *.bc impl_AIGs spec_AIGs
distclean: clean
	$(RM) -r $(libgcrypt)
	$(RM) $(libgcrypt_pkg)

.PRECIOUS: %.bc
.PHONY: all aigs clean distclean prep prove

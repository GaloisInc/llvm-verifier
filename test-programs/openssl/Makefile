LSS?=../../cabal-dev/bin/lss
CLANG?=clang

all: aes.aig

%.bc : %.c
	${CLANG} -emit-llvm -c -o $@ $<

aes.bc: aes_core.bc aes_test.bc
	llvm-link -o $@ $^

aes.aig: aes.bc
	${LSS} $<

clean:
	rm -f *.bc *.aig

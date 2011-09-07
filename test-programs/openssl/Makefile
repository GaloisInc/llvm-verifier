LSS?=../../cabal-dev/bin/lss

all: aes.aig

%.bc : %.c
	clang -emit-llvm -c -o $@ $<

aes.bc: aes_core.bc aes_test.bc
	llvm-link -o $@ $^

aes.aig: aes.bc
	${LSS} $<

clean:
	rm -f *.bc *.aig

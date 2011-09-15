CC=gcc
CCFLAGS?=-Wall -O3 -DNDEBUG -fomit-frame-pointer -I../../../sym-api
LSS?=../../bin/lss
SYMAPI=./sym-api

all: aes128BlockEncrypt_driver_llvm aes.bc

%.bc: %.c
	clang -emit-llvm -I${SYMAPI} -c $< -o $@

sym-api.bc: ${SYMAPI}/sym-api.c
	clang -emit-llvm -I${SYMAPI} -c $< -o $@

aes.bc: aes128BlockEncrypt.bc aes128BlockEncrypt_driver.bc
	llvm-link $^ -o $@

aes.aig: aes.bc
	${LSS} $<

aes128BlockEncrypt_driver_llvm: aes128BlockEncrypt.bc aes128BlockEncrypt_driver.bc sym-api.bc
	llvm-dis aes128BlockEncrypt.bc
	llvm-dis aes128BlockEncrypt_driver.bc
	llvm-ld -disable-opt -native $^ -o $@

check: aes.aig
	cryptol -b ref/equivAES.scr

clean:
	rm -f *.o *.bc *.ll *.s aes128BlockEncrypt_driver_llvm aes.aig

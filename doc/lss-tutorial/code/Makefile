TOP=../../..
SYMAPI=${TOP}/sym-api
CC=gcc
CCFLAGS?=-Wall -O3 -DNDEBUG -fomit-frame-pointer -I${SYMAPI}
LSS?=../../bin/lss

all: aes.bc

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
	rm -f *.o *.bc *.ll *.s aes128BlockEncrypt_driver_llvm aes.aig \
	noleaks.cnf

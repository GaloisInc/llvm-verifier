all : trivial.ll trivial-opt.ll multiexit.ll multiexit-opt.ll branch.ll branch-opt.ll alloc.ll alloc-opt.ll exception.ll exception-opt.ll openssl/aes_test

bc : trivial.bc trivial-opt.bc multiexit.bc multiexit-opt.bc branch.bc ibranch-opt.bc alloc.bc alloc-opt.bc exception.bc exception-opt.bc

# NB: We only emit .dot files for unoptimized CFGs
CLANG ?= clang

%.bc : %.cpp
	$(CLANG) -c -emit-llvm -o $@ $^
	opt -dot-cfg $@ > /dev/null

%.o : %.c
	$(CLANG) -c -emit-llvm -o $@ $^

%.bc : %.c
	$(CLANG) -c -emit-llvm -o $@ $^
	opt -dot-cfg $@  > /dev/null

%-opt.bc : %.bc
	opt -O3 -o $@ $^

%.ll : %.bc
	llvm-dis $^

openssl/aes_test : openssl/aes_core.o openssl/aes_test.o
	llvm-ld -o $@ $^

openssl/sha256.o : openssl/sha256.c openssl/md32_common.h
	$(CLANG) -c -D OPENSSL_NO_ASM -Wno-unused-value -emit-llvm -o $@ openssl/sha256.c

openssl/sha224_test : openssl/sha224_test.o openssl/sha256.o openssl/mem_clr.o
	llvm-ld -o $@ $^

openssl/casttest.bc : openssl/casttest.c
	$(CLANG) -c -emit-llvm -m32 -o $@ $^

openssl/c_skey.bc : openssl/c_skey.c
	$(CLANG) -c -emit-llvm -m32 -o $@ $^

openssl/c_ecb.bc : openssl/c_ecb.c
	$(CLANG) -c -emit-llvm -m32 -o $@ $^

openssl/c_enc.bc : openssl/c_enc.c
	$(CLANG) -c -emit-llvm -m32 -o $@ $^

openssl/cast_test : openssl/casttest.bc openssl/c_skey.bc openssl/c_enc.bc openssl/c_ecb.bc
	llvm-ld -o $@ $^

clean : 
	rm -f *.bc *.ll *.dot *.o openssl/*.o openssl/*.bc openssl/aes_test openssl/sha224_test openssl/cast_test

.PHONY : clean all bc

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

clean : 
	rm -f *.bc *.ll *.dot *.o openssl/*.o openssl/*.bc openssl/aes_test

.PHONY : clean all bc

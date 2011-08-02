all : branch.ll branch-opt.ll alloc.ll alloc-opt.ll exception.ll exception-opt.ll openssl/aes_test

# NB: We only emit .dot files for unoptimized CFGs

%.bc : %.cpp
	clang -c -emit-llvm -o $@ $^
	opt -dot-cfg $@ > /dev/null

%.o : %.c
	clang -c -emit-llvm -o $@ $^

%.bc : %.c
	clang -c -emit-llvm -o $@ $^
	opt -dot-cfg $@  > /dev/null

%-opt.bc : %.bc
	opt -O3 -o $@ $^

%.ll : %.bc
	llvm-dis $^

openssl/aes_test : openssl/aes_core.o openssl/aes_test.o
	llvm-ld -o $@ $^

clean : 
	rm -f *.bc *.ll *.dot *.o openssl/*.o openssl/*.bc openssl/aes_test

.PHONY : clean all

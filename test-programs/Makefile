all : branch.ll branch-opt.ll alloc.ll alloc-opt.ll exception.ll exception-opt.ll 

%.bc : %.cpp
	clang -c -emit-llvm -o $@ $^

%.bc : %.c
	clang -c -emit-llvm -o $@ $^

%-opt.bc : %.bc
	opt -O3 -o $@ $^

%.ll : %.bc
	llvm-dis $^


clean : 
	rm -f *.bc *.ll

.PHONY : clean all

all : trivial.ll trivial-opt.ll multiexit.ll multiexit-opt.ll branch.ll branch-opt.ll alloc.ll alloc-opt.ll exception.ll exception-opt.ll 

# NB: We only emit .dot files for unoptimized CFGs

%.bc : %.cpp
	clang -c -emit-llvm -o $@ $^
	opt -dot-cfg $@ > /dev/null

%.bc : %.c
	clang -c -emit-llvm -o $@ $^
	opt -dot-cfg $@  > /dev/null

%-opt.bc : %.bc
	opt -O3 -o $@ $^

%.ll : %.bc
	llvm-dis $^


clean : 
	rm -f *.bc *.ll *.dot

.PHONY : clean all

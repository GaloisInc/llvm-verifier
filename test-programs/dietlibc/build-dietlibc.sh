cd dietlibc-0.32/lib
for f in *.c ; do clang -nostdinc -emit-llvm -I../include -I.. -c $f ; done
for f in *.o ; do mv $f `basename $f .o`.bc ; done
llvm-link *.bc -o dietlibc-0.32.bc

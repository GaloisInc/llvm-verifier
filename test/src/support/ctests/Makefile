CFILES=$(wildcard *.c)
LLFILES=$(wildcard *.ll)
BCFILES=$(patsubst %.c,%.bc,$(CFILES)) $(patsubst %.ll,%.bc,$(LLFILES))

all: $(BCFILES)

%.ll : %.c
	clang -emit-llvm -I ../../../../sym-api -S $< -o $@

%.bc : %.c
	clang -emit-llvm -I ../../../../sym-api -c $< -o $@

%.bc : %.ll
	llvm-as $<

clean:
	rm -f $(BCFILES)

CFILES=$(wildcard *.c)
LLFILES=$(wildcard *.ll)
BCFILES=$(patsubst %.c,%.bc,$(CFILES)) $(patsubst %.ll,%.bc,$(LLFILES))
CLANG?=clang

all: $(BCFILES)

%.ll : %.c
	$(CLANG) -emit-llvm -I ../../../../sym-api -S $< -o $@

%.bc : %.c
	$(CLANG) -emit-llvm -I ../../../../sym-api -c $< -o $@

%.bc : %.ll
	llvm-as $<

clean:
	rm -f $(BCFILES)

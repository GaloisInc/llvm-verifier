CFILES=$(wildcard *.c)
LLFILES=$(wildcard *.ll)
BCFILES=$(patsubst %.c,%.bc,$(CFILES)) $(patsubst %.ll,%.bc,$(LLFILES))

all: $(BCFILES)

%.ll : %.c
	clang -emit-llvm -S $< -o $@

%.bc : %.ll
	llvm-as $<

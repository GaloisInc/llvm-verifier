
.PHONY: all
all:

.PHONY: clean
clean:

libcxx     := libcxx
libcxx-svn := http://llvm.org/svn/llvm-project/libcxx/trunk

libcxx/Makefile:
	svn co $(libcxx-svn) $(libcxx)

.PHONY: distclean
distclean: clean
	$(RM) -r $(libcxx)

all: bitcode
.PHONY: bitcode
bitcode: libcxx/Makefile
	for i in `find $(libcxx)/src -name \*.cpp`; do \
	  clang -c -I$(libcxx)/include -c $$i -emit-llvm \
	    -o `echo $$(basename $$i) | sed 's|\(.*\)\.cpp|\1.bc|'`; \
	done

clean: clean-bitcode
.PHONY: clean-bitcode
clean-bitcode:
	$(RM) *.bc

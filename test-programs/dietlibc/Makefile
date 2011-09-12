CLANG?=clang
VERSION=0.32
TARBALL=dietlibc-$(VERSION).tar.bz2
SRCDIR=dietlibc-$(VERSION)

.PHONY: all
all: $(SRCDIR).bc

.PHONY: clean
clean:

$(TARBALL):
	wget http://www.kernel.org/pub/linux/libs/dietlibc/$(TARBALL)

$(SRCDIR)/Makefile: $(TARBALL)
	tar -xjvf $(TARBALL)

.PHONY: distclean
distclean: clean
	$(RM) -rf $(TARBALL) $(SRCDIR)

.PHONY: bitcode
bitcode: $(SRCDIR)/Makefile
	for i in `find $(SRCDIR)/lib -name \*.c | grep -v exit`; do \
	  $(CLANG) -c -I$(SRCDIR)/include -I$(SRCDIR) -c $$i -emit-llvm \
	    -o `echo $$i | sed 's|\(.*\)\.c|\1.bc|'`; \
	done

$(SRCDIR).bc: bitcode
	llvm-link -o $@ `find $(SRCDIR)/lib -name \*.bc`

clean: clean-bitcode
.PHONY: clean-bitcode
clean-bitcode:
	$(RM) -f $(SRCDIR)/lib/*.bc

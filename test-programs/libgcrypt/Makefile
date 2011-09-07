
libgcrypt     := libgcrypt-1.5.0
libgcrypt_pkg := $(libgcrypt).tar.bz2
libgcrypt_url ?= ftp://ftp.gnupg.org/gcrypt/libgcrypt/$(libgcrypt_pkg)

CC := clang
CFLAGS := -I$(libgcrypt) -I$(libgcrypt)/src -I$(libgcrypt)/cipher -Wall

.PHONY: all
all: test test.elf

%.bc: %.c
	$(CC) $(CFLAGS) -emit-llvm -c -o $@ $<



$(libgcrypt_pkg):
	curl $(libgcrypt_url) > $@

$(libgcrypt)/.token: $(libgcrypt_pkg)
	tar -jxvf $<
	touch $@

$(libgcrypt)/config.h: $(libgcrypt)/.token
	cd $(libgcrypt); ./configure


test_objects := sha512.bc sha-384-driver.bc
test: $(libgcrypt)/config.h $(test_objects)
	llvm-ld -o $@ $(test_objects)

test.elf_objects := sha512.o sha-384-driver.o
test.elf: $(libgcrypt)/config.h $(test.elf_objects)
	$(CC) $(LDFLAGS) -o $@ $(test.elf_objects)


.PHONY: clean
clean:
	$(RM) *.bc test
	$(RM) *.o test.elf

.PHONY: distclean
distclean: clean
	$(RM) -r $(libgcrypt)
	$(RM) $(libgcrypt_pkg)

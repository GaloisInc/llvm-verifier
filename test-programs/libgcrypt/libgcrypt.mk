libgcrypt               := libgcrypt-1.5.0
libgcrypt_pkg           := $(libgcrypt).tar.bz2
libgcrypt_url           ?= ftp://ftp.gnupg.org/gcrypt/libgcrypt/$(libgcrypt_pkg)
libgcrypt_extra_include := /opt/local/include
libgcrypt_flags         := -I$(libgcrypt) -I$(libgcrypt)/src -I$(libgcrypt)/cipher -I$(libgcrypt_extra_include) 

$(libgcrypt_pkg):
	curl $(libgcrypt_url) > $@

$(libgcrypt)/.token: $(libgcrypt_pkg)
	tar -jxvf $<
	touch $@

$(libgcrypt)/config.h: $(libgcrypt)/.token
	cd $(libgcrypt); ./configure --enable-ciphers=aes \
		--enable-digests=sha512 --enable-pubkey-ciphers=rsa \
		--disable-aesni-support --disable-padlock-support

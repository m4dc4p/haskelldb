TOP_DIR = .

SUBDIRS = src doc

include $(TOP_DIR)/rules.mk

.PHONY: distclean maintainer-clean

all: src

configure: configure.ac aclocal.m4
	autoconf

config.status: configure
	./config.status --recheck

config.mk: config.mk.in config.status
	./config.status

haskelldb.pkg: haskelldb.pkg.in config.status
	./config.status

distclean: clean
	-rm -f config.status config.mk config.log 
	-rm -rf autom4te.cache

maintainer-clean: distclean

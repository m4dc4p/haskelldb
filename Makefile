TOP_DIR = .

SUBDIRS = src doc test

include $(TOP_DIR)/rules.mk

DIST_DIR = haskelldb-$(PACKAGE_VERSION)

INSTALL_COMPILERS = $(addprefix install-,$(COMPILERS))
INSTALL_FILESONLY_COMPILERS = $(addprefix install-filesonly-,$(COMPILERS))
UNINSTALL_COMPILERS = $(addprefix uninstall-,$(COMPILERS))

.PHONY: install $(INSTALL_COMPILERS) $(INSTALL_FILESONLY_COMPILERS) \
	uninstall $(UNINSTALL_COMPILERS) \
	dist distclean maintainer-clean

all: src

configure: configure.ac aclocal.m4
	autoconf

config.status: configure
	./config.status --recheck

config.mk: config.mk.in config.status
	./config.status

haskelldb.pkg: haskelldb.pkg.in config.status
	./config.status

install: all $(INSTALL_COMPILERS)

install-ghc: all haskelldb.pkg install-filesonly-ghc
	-cd $(GHC_DIR); rm -f HShdb.o
	$(GHC_PKG) -u --auto-ghci-libs -i haskelldb.pkg

install-filesonly-ghc: all
	cd build; tar -cf ghc-interfaces.tar `find Database -name '*.hi' -print`
	cp build/ghc-interfaces.tar $(GHC_DIR)/imports
	cd $(GHC_DIR)/imports; tar -xf ghc-interfaces.tar; rm -f ghc-interfaces.tar
	cd build; cp libHShdb.a $(GHC_DIR)

install-hugs: all
	cd build; tar -cf hugs-libraries.tar `find Database -name '*.hs' -print`
	cp build/hugs-libraries.tar $(HUGS_DIR)/libraries
	cd $(HUGS_DIR)/libraries; tar -xf hugs-libraries.tar; rm -f hugs-libraries.tar

uninstall: $(UNINSTALL_COMPILERS)

uninstall-ghc:
	-cd $(GHC_DIR)/imports; rm -rf Database/HaskellDB*
	-cd $(GHC_DIR); rm -f libHShdb.a HShdb.o
	-$(GHC_PKG) -r haskelldb

uninstall-hugs:
	-cd $(HUGS_DIR)/libraries; rm -rf Database/HaskellDB*

clean:
	-rm -rf $(BUILD_DIR)/*

dist:
	mkdir $(DIST_DIR)
	cvs export -d $(DIST_DIR) -rHEAD hwt_haskelldb
	cd $(DIST_DIR) && autoconf && rm -rf autom4te.cache
	find $(DIST_DIR) -name .cvsignore -exec rm -f {} ';'
	tar -zcf $(DIST_DIR).tar.gz $(DIST_DIR)
	zip -r $(DIST_DIR).zip $(DIST_DIR)
	rm -rf $(DIST_DIR)

distclean: clean
	-rm -f config.status config.mk config.log 
	-rm -rf autom4te.cache

maintainer-clean: distclean
	-rm -f *.tar.gz *.zip

TOP_DIR = .

SUBDIRS = doc test

include $(TOP_DIR)/rules.mk

DIST_DIR = haskelldb-$(PACKAGE_VERSION)

ALL_COMPILERS               = $(addprefix all-,$(COMPILERS))
PROGRAMS_COMPILERS           = $(addprefix programs-,$(COMPILERS))
INSTALL_COMPILERS           = $(addprefix install-,$(COMPILERS))
INSTALL_FILESONLY_COMPILERS = $(addprefix install-filesonly-,$(COMPILERS))
UNINSTALL_COMPILERS         = $(addprefix uninstall-,$(COMPILERS))
CLEAN_COMPILERS             = $(addprefix clean-,$(COMPILERS))

.PHONY: all programs install uninstall \
	dist distclean maintainer-clean \
        $(ALL_COMPILERS) \
        $(PROGRAMS_COMPILERS) \
        $(INSTALL_COMPILERS) \
        $(INSTALL_FILESONLY_COMPILERS) \
	$(UNINSTALL_COMPILERS) \
        $(CLEAN_COMPILERS)

$(ALL_COMPILERS):
	$(MAKE) -f $(subst all-,Makefile., $@) all

$(PROGRAMS_COMPILERS):
	$(MAKE) -f $(subst programs-,Makefile., $@) all

$(INSTALL_COMPILERS):
	$(MAKE) -f $(subst install-,Makefile., $@) install

$(INSTALL_FILESONLY_COMPILERS):
	$(MAKE) -f $(subst install-filesonly-,Makefile., $@) install-filesonly

$(UNINSTALL_COMPILERS):
	$(MAKE) -f $(subst uninstall-,Makefile., $@) uninstall

$(CLEAN_COMPILERS):
	$(MAKE) -f $(subst clean-,Makefile., $@) clean

default all: $(ALL_COMPILERS)

programs: $(PROGRAMS_COMPILERS)

configure: configure.ac aclocal.m4
	autoconf

config.status: configure
	./config.status --recheck

# Causes autoconf and configure to be run when doing clean
#config.mk: config.mk.in config.status
#	./config.status

# Should be in Makefile.ghc
haskelldb.pkg: haskelldb.pkg.in config.status
	./config.status

install: all $(INSTALL_COMPILERS)

uninstall: $(UNINSTALL_COMPILERS)

clean: $(CLEAN_COMPILERS)
	-rm -rf $(BUILD_DIR)

dist:
	mkdir $(DIST_DIR)
	cvs export -d $(DIST_DIR) -rHEAD haskelldb
	cd $(DIST_DIR) && autoconf && rm -rf autom4te.cache
	find $(DIST_DIR) -name .cvsignore -exec rm -f {} ';'
	tar -zcf $(DIST_DIR).tar.gz $(DIST_DIR)
	zip -r $(DIST_DIR).zip $(DIST_DIR)
	rm -rf $(DIST_DIR)

distclean: clean
	-rm -f config.status config.mk config.log haskelldb.pkg
	-rm -rf autom4te.cache

maintainer-clean: distclean
	-rm -f *.tar.gz *.zip

TOP_DIR = .

include $(TOP_DIR)/config.mk
include $(TOP_DIR)/rules.mk

SUBDIRS = src doc

SUBDIRS_CLEAN = $(addsuffix -clean, $(SUBDIRS))

.PHONY: all doc clean $(SUBDIRS_CLEAN)

default all: 
	$(MAKE) -C src
doc: 
	$(MAKE) -C doc

clean: $(SUBDIRS_CLEAN)

$(SUBDIRS_CLEAN):
	$(MAKE) -C $(subst -clean,,$@) clean

SUBDIRS = src doc

SUBDIRS_CLEAN = $(addsuffix -clean, $(SUBDIRS))

.PHONY = haskelldb doc clean $(SUBDIRS_CLEAN)

default haskelldb: 
	$(MAKE) -C src

doc: 
	$(MAKE) -C doc

clean: $(SUBDIRS_CLEAN)

$(SUBDIRS_CLEAN):
	$(MAKE) -C $(subst -clean,,$@) clean

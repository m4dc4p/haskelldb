include $(TOP_DIR)/config.mk

SUBDIRS_CLEAN = $(addsuffix -clean, $(SUBDIRS))

.PHONY: default all $(SUBDIRS) clean $(SUBDIRS_CLEAN)

default: all

hugsload-%: %.hs
	$(HUGS) $(HUGSFLAGS) $^

hugsrun-%: %.hs
	$(RUNHUGS) $(HUGSFLAGS) $^

ghciload-%: %.hs
	$(GHCI) $(GHCFLAGS) $(LDFLAGS) $^

$(SUBDIRS):
	$(MAKE) -C $@

clean: $(SUBDIRS_CLEAN)

$(SUBDIRS_CLEAN):
	$(MAKE) -C $(subst -clean,,$@) clean


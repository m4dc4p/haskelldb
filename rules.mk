include $(TOP_DIR)/config.mk

export GHCFLAGS_WARN

SUBDIRS_CLEAN = $(addsuffix -clean, $(SUBDIRS))

.PHONY: default all $(SUBDIRS) clean $(SUBDIRS_CLEAN) \
	hugsload hugstest ghciload

default: all

warn-%: GHCFLAGS_WARN += $(GHC_VERBOSE_WARNINGS)

hugsload: 
	$(HUGS) $(HUGSFLAGS) $^

hugstest: 
	$(RUNHUGS) $(HUGSFLAGS) $^

ghciload: 
	$(GHCI) $(GHCFLAGS) $(LDFLAGS) $^

%: %.o
	$(GHC) $(GHCFLAGS) $^ $(LDFLAGS) -o $@

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $<

%.hi: %.o
	@\:

.depend: 
	$(GHC) -M -optdep-f -optdep.depend $(GHCFLAGS) $^

$(SUBDIRS):
	$(MAKE) -C $@

clean: $(SUBDIRS_CLEAN)

$(SUBDIRS_CLEAN):
	$(MAKE) -C $(subst -clean,,$@) clean

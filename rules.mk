export GHCFLAGS_WARN

warn-%: GHCFLAGS_WARN += $(GHC_VERBOSE_WARNINGS)

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $<

%.hi: %.o
	@\:
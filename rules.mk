%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $<

%.hi: %.o
	@\:
.SUFFIXES: .hs .hi .o

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

%.hi: %.o
	@\:
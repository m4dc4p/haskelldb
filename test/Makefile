GHC = ghc
GHCFLAGS = -fglasgow-exts -fcontext-stack64

.PHONY: runtests tests dbmodules clean

runtests: tests
	@echo "Did you remember to edit test.config?"
	@echo "Running tests..."
	./tests

tests: dbmodules
	$(GHC) $(GHCFLAGS) --make -o $@ RunTests.hs

createmodules: DescDB1.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

dbmodules: createmodules
	./createmodules

clean:
	-rm -f *.hi *.o DB1/*.hi DB1/*.hi
	-rm -f createmodules tests

genclean: clean
	-rm -rf DB1 DB1.hs
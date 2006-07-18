GHC = ghc
GHCFLAGS = -fcontext-stack64

.PHONY: runtests clean

runtests: tests
	@echo "Did you remember to edit test.config?"
	@echo "Running tests..."
	./tests

tests: RunTests.hs DBTest.hs TestCases.hs DB1/Hdb_t1.hs DB1/Hdb_t2.hs
	$(GHC) $(GHCFLAGS) --make -o $@ RunTests.hs

createmodules: DescDB1.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

DB1.hs DB1/Hdb_t1.hs DB1/Hdb_t2.hs: createmodules
	./createmodules

clean:
	-rm -f *.hi *.o DB1/*.hi DB1/*.hi
	-rm -f createmodules tests

genclean: clean
	-rm -rf DB1 DB1.hs
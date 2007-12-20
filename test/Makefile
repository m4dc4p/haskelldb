GHC = ghc
GHCFLAGS = -fglasgow-exts -fcontext-stack64

DRIVERS = \
 flat \
 hdbc-postgresql \
 hdbc-sqlite3 \
 hsql-mysql \
 hsql-postgresql \
 hsql-sqlite \
 hsql-sqlite3

.PHONY: runtests tests dbmodules clean

runtests: tests
	@echo "Running tests..."
	for d in $(DRIVERS); do ./test-$$d; done

tests: dbmodules
	@echo "Compiling tests..."
	-for d in $(DRIVERS); do $(GHC) $(GHCFLAGS) --make -o test-$$d test-$$d.hs; done

dbmodules: createmodules
	./createmodules

createmodules: DescDB1.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

clean:
	-rm -f *.hi *.o DB1/*.hi DB1/*.hi
	-rm -f createmodules
	-for d in $(DRIVERS); do rm test-$$d; done

genclean: clean
	-rm -rf DB1 DB1.hs

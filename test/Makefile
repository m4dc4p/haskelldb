PROG = hardcoded-layout-simple-query join-and-aggr html-users-list \
       big-test big-test-create-tables current-time date-types \
       insert-update-delete test-bounded transaction

.PHONY: all

all: $(PROG)

%: %.hs
	ghc --make -fcontext-stack30 -o $@ $^

clean:
	-rm -f $(PROG) *.o *.hi 
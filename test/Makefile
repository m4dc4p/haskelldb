PROG = hardcoded-layout-simple-query join-and-aggr html-users-list

.PHONY: all

all: $(PROG)

%: %.hs
	ghc --make -fcontext-stack30 -o $@ $^

clean:
	-rm -f $(PROG) *.o *.hi 
PROG = hardcoded-layout-simple-query join-and-aggr html-users-list

.PHONY: all

all: $(PROG)

%: %.hs
	ghc --make -o $@ $^

clean:
	-rm -f $(PROG) *.o *.hi 
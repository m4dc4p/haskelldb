
.PHONY: clean install

all: DBDirect

DBDirect: src/DBDirect.hs
	ghc -package haskelldb-dynamic --make -o $@ $^

clean:
	-rm -f *.o *.hi
	-rm -f DBDirect

install:
	install DBDirect /usr/local/bin
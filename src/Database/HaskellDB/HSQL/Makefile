# in HSQL_DIR there should be a directory (or a symlink to a directory)
# called Database that contains the compiled HSQL library
HSQL_DIR=/users/dtek/d00/d00bring/d3proj

LDFLAGS=-L$(HSQL_DIR)/Database -lHSsql -lodbc
CFLAGS=
GHC=ghc
GHCFLAGS=-fglasgow-exts -fallow-overlapping-instances -i../.. -i$(HSQL_DIR)

HUGS=hugs
RUNHUGS=runhugs
HUGSFLAGS=-98 +o -P:../..:$(HSQL_DIR)


.PHONY: clean

default all: test

#
# HUGS
#

hugsload: 
	$(HUGS) $(HUGSFLAGS) HSQL_driver.hs

hugstest: 
	$(RUNHUGS) $(HUGSFLAGS) test.hs

#
# GHC
#

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

%.hi: %.o
	@\:

test.o: HSQL_driver.hi

test: test.o HSQL_driver.o
	$(GHC) $(GHCFLAGS) -o $@ $^ $(LDFLAGS) -L../.. -lHDB

#
# Generic
#

clean:
	-rm -f test *.so *.o *.hi

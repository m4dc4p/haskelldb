HSQL_DIR=/users/dtek/d00/d00bring/d3proj
LDFLAGS=-L$(HSQL_DIR)/HSQL -lHSsql -lodbc
CFLAGS=
GHC=ghc
GHCFLAGS=-fglasgow-exts -fallow-overlapping-instances -i../.. -i$(HSQL_DIR)
HUGS=hugs
RUNHUGS=runhugs
FFIHUGS=ffihugs
HUGSFLAGS=-98 +o -P:../..


.PHONY: clean

default all: test

#
# HUGS
#

ODBC.so: ODBC.hs ODBCConst.hs
	$(FFIHUGS) $(HUGSFLAGS) +G +L"$(CFLAGS)" +L"$(LDFLAGS)" ODBC.hs

hugsload: ODBC.so
	$(HUGS) $(HUGSFLAGS) ODBC.hs

hugsrun: ODBC.so
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

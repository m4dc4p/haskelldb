# in HSQL_DIR there should be a directory (or a symlink to a directory)
# called Database that contains the compiled HSQL library
HSQL_DIR=.

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

hugsload: $(HSQL_DIR)/Database/ODBC/HSQL.hs  $(HSQL_DIR)/Database/ODBC/HSQL.so
	$(HUGS) $(HUGSFLAGS) HSQL_driver.hs

hugstest: $(HSQL_DIR)/Database/ODBC/HSQL.hs  $(HSQL_DIR)/Database/ODBC/HSQL.so
	$(RUNHUGS) $(HUGSFLAGS) test.hs

#
# GHC
#

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

%.hi: %.o
	@\:

HSQL_driver.o: $(HSQL_DIR)/Database/ODBC/HSQL.hi

test.o: HSQL_driver.hi

test: $(HSQL_DIR)/Database/libHSsql.a test.o HSQL_driver.o
	$(GHC) $(GHCFLAGS) -o $@ $^ $(LDFLAGS) -L../.. -lHDB

#
# Generic
#

HSQL/% Database/%: HSQL-desc.patch HSQL-hugs.patch
	$(MAKE) -f Makefile.HSQL HSQL Database

clean:
	-rm -f test *.so *.o *.hi
	$(MAKE) -f Makefile.HSQL clean

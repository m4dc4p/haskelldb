include ../../config.mk
include ../../rules.mk

# in HSQL_DIR there should be a directory (or a symlink to a directory)
# that contains a directory called Database 
# that contains the compiled HSQL library
HSQL_DIR = .

LDFLAGS += -L$(HSQL_DIR)/Database -lHSsql -lodbc
GHCFLAGS += -i../.. -i$(HSQL_DIR)

HUGSFLAGS += -P:../..:$(HSQL_DIR)

OBJS = HSQL_driver.o test.o

SRC = $(patsubst %.o,%.hs, $(OBJS))

.PHONY: clean HSQL

default all: test

#
# HUGS
#

hugsload: $(HSQL_DIR)/Database/ODBC/HSQL.hs $(HSQL_DIR)/Database/ODBC/HSQL.so
	$(HUGS) $(HUGSFLAGS) test.hs

hugstest: $(HSQL_DIR)/Database/ODBC/HSQL.hs $(HSQL_DIR)/Database/ODBC/HSQL.so
	$(RUNHUGS) $(HUGSFLAGS) test.hs

#
# GHC
#

HSQL:
	$(MAKE) -f Makefile.HSQL HSQL/libHSsql.a

$(HSQL_DIR)/Database/libHSsql.a: HSQL

test: $(HSQL_DIR)/Database/libHSsql.a $(OBJS)
	$(GHC) $(GHCFLAGS) -o $@ $^ $(LDFLAGS) -L../.. -lHDB

ghciload: $(HSQL_DIR)/Database/ODBC/HSQL.hs
	$(GHCI) $(GHCFLAGS) test.hs

#
# Generic
#

../../libHSsql.a: 
	$(MAKE) -C ../.. libHSsql.a

.depend: $(SRC) 
	$(GHC) -M -optdep-f -optdep.depend $(GHCFLAGS) $^

clean:
	-rm -f test *.so *.o *.hi .depend .depend.bak

include .depend

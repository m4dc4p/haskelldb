include ../../config.mk
include ../../rules.mk

LDFLAGS += -L$(HSQL_DIR)/Database -L$(UNIXODBC_LIBDIR)/lib -lHSsql -lodbc -lHDB
GHCFLAGS += -i../.. -i$(HSQL_DIR)

HUGSFLAGS += -P:../..:$(HSQL_DIR)

OBJS = HSQL_driver.o test.o

SRC = $(patsubst %.o,%.hs, $(OBJS))

.PHONY: clean hugsload hugstest ghciload

default all: test

#
# HUGS
#

hugsload: 
	$(HUGS) $(HUGSFLAGS) test.hs

hugstest: 
	$(RUNHUGS) $(HUGSFLAGS) test.hs

#
# GHC
#

test: $(OBJS)
	$(GHC) $(GHCFLAGS) -o $@ $^ $(LDFLAGS) -L../..

ghciload: 
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

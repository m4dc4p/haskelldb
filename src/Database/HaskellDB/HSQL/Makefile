include ../../config.mk
include ../../rules.mk

LIBDIRS = ../.. $(HSQL_DIR)/Database $(UNIXODBC_LIBDIR)/lib
LIBS = HDB HSsql odbc 

LDFLAGS += $(addprefix -L, $(LIBDIRS)) $(addprefix -l, $(LIBS))
GHCFLAGS += -i../.. -i$(HSQL_DIR)

HUGSFLAGS += -P:../..:$(HSQL_DIR)

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

test: test.o
	$(GHC) $(GHCFLAGS) -o $@ $^ $(LDFLAGS)

ghciload: 
	$(GHCI) $(GHCFLAGS) test.hs

#
# Generic
#

../../libHSsql.a: 
	$(MAKE) -C ../.. libHSsql.a

.depend: *.hs
	$(GHC) -M -optdep-f -optdep.depend $(GHCFLAGS) $^

clean:
	-rm -f test *.so *.o *.hi .depend .depend.bak

include .depend

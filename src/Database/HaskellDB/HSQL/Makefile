include ../../config.mk
include ../../rules.mk

LDFLAGS += -L../.. -lHDB -lHSsql -lodbc 
GHCFLAGS += -i../..

HUGSFLAGS += -P:$(HSQL_DIR)

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

.depend: *.hs
	$(GHC) -M -optdep-f -optdep.depend $(GHCFLAGS) $^

clean:
	-rm -f test *.so *.o *.hi .depend .depend.bak

include .depend

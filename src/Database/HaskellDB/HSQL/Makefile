TOP_DIR = ../../..

include $(TOP_DIR)/config.mk
include $(TOP_DIR)/rules.mk

LDFLAGS += -lHDB -lHSsql -lodbc 

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

TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

LDFLAGS += -lHDB -lHSsql -lodbc 

all: test

hugsload: test.hs
hugstest: test.hs
ghciload: test.hs

test: test.o

.depend: *.hs

clean:
	-rm -f test *.so *.o *.hi .depend .depend.bak

include .depend

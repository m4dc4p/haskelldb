TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

LDFLAGS += -lHDB -lHSsql -lodbc 

all: test

hugsload: test.hs
hugstest: test.hs
ghciload: LDFLAGS = -lodbc 
ghciload: test.hs

test: test.o $(SRC_DIR)/libHDB.a

.depend: *.hs

clean:
	-rm -f test *.so *.o *.hi .depend .depend.bak

include .depend

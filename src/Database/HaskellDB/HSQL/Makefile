TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

LDFLAGS += -lHDB -lHSsql -lodbc 

DRIVERS = HSQL_ODBC HSQL_MySQL HSQL_PostgreSQL

.PHONY: drivers $(DRIVERS)

all: drivers

drivers: $(DRIVERS)

HSQL_ODBC: HSQL_ODBC.o 
HSQL_MySQL: HSQL_MySQL.o 
HSQL_PostgreSQL: HSQL_PostgreSQL.o 

hugsload: test.hs
hugstest: test.hs
ghciload: LDFLAGS = -lodbc 
ghciload: test.hs

test: test.o $(SRC_DIR)/libHDB.a

.depend: *.hs

clean:
	-rm -f test *.so *.o *.hi .depend .depend.bak

include .depend

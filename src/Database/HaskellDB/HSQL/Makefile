TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

.PHONY: drivers $(HSQL_DRIVERS)

all: drivers

drivers: $(HSQL_DRIVERS)

HSQL_ODBC: HSQL_ODBC.o 
HSQL_MySQL: HSQL_MySQL.o 
HSQL_PostgreSQL: HSQL_PostgreSQL.o 

.depend: *.hs

clean:
	-rm -f *.so *.o *.hi .depend .depend.bak

include .depend

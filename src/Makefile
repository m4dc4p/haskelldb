TOP_DIR = ..

include $(TOP_DIR)/rules.mk

OBJS = Database.o HaskellDB.o HDBRec.o HDBRecUtils.o Optimize.o \
       PrimQuery.o Query.o Sql.o FieldType.o

DRIVERS = hsql

# FIXME: we shouldn't have to list these here, do it 
# somewhere in each driver directory
DRIVERS_OBJS = drivers/hsql/HSQL_driver.o \
               drivers/hsql/HSQL_ODBC.o \
               drivers/hsql/HSQL_MySQL.o

SRC = $(patsubst %.o,%.hs, $(OBJS))

DRIVER_DIRS = $(addprefix drivers/, $(DRIVERS))
DRIVERS_TEST = $(addsuffix -test, $(DRIVER_DIRS))
DRIVERS_CLEAN = $(addsuffix -clean, $(DRIVER_DIRS))

.PHONY: drivers $(DRIVER_DIRS) $(DRIVERS_OBJS) \
	dbdirect dbdirect-clean test $(DRIVERS_TEST) \
	$(DRIVERS_CLEAN)

all: $(OBJS) drivers libHDB.a dbdirect

warn-all: all

libHDB.a: $(OBJS) $(DRIVERS_OBJS)
	$(AR) -rc $@ $^

drivers: $(DRIVER_DIRS)

$(DRIVER_DIRS):
	$(MAKE) -C $@

dbdirect:
	$(MAKE) -C dbdirect

test: $(DRIVERS_TEST)

$(DRIVERS_TEST):
	$(MAKE) -C $(subst -test,,$@) test

# debugging 

hugsload: HaskellDB.hs
ghciload: HaskellDB.hs

# cleaning up

clean: dbdirect-clean $(DRIVERS_CLEAN)
	-rm -f *.o *.hi *.a .depend .depend.bak

dbdirect-clean:
	$(MAKE) -C dbdirect clean

$(DRIVERS_CLEAN):
	$(MAKE) -C $(subst -clean,,$@) clean

# dependencies

.depend: $(SRC)

include .depend

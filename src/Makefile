TOP_DIR = ..

include $(TOP_DIR)/rules.mk

OBJS = Database.o HaskellDB.o HDBRec.o HDBRecUtils.o Optimize.o \
       PrimQuery.o Query.o Sql.o FieldType.o

DRIVER_OBJS = drivers/hsql/HSQL_driver.o

SRC = $(patsubst %.o,%.hs, $(OBJS))

DRIVER_DIRS = $(dir $(DRIVER_OBJS))
DRIVERS_TEST = $(addsuffix -test, $(DRIVER_DIRS))
DRIVERS_CLEAN = $(addsuffix -clean, $(DRIVER_DIRS))

.PHONY: drivers $(DRIVER_OBJS)  \
	dbdirect dbdirect-clean test $(DRIVERS_TEST) \
	$(DRIVERS_CLEAN)

all: $(OBJS) drivers libHDB.a dbdirect

warn-all: all

libHDB.a: $(OBJS) $(DRIVER_OBJS)
	$(AR) -rc $@ $^

drivers: $(DRIVER_OBJS)

$(DRIVER_OBJS):
	$(MAKE) -C $(dir $@) $(notdir $@)

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

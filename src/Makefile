TOP_DIR = ..

include $(TOP_DIR)/config.mk
include $(TOP_DIR)/rules.mk

OBJS = Database.o HaskellDB.o HDBRec.o HDBRecUtils.o Optimize.o PPrint.o \
       PrimQuery.o Query.o Sql.o FieldType.o

DRIVER_OBJS = drivers/hsql/HSQL_driver.o


SRC = $(patsubst %.o,%.hs, $(OBJS))

DRIVER_DIRS = $(dir $(DRIVER_OBJS))
DRIVERS_TEST = $(addsuffix -test, $(DRIVER_DIRS))
DRIVERS_CLEAN = $(addsuffix -clean, $(DRIVER_DIRS))

.PHONY: all clean drivers $(DRIVERS_CLEAN) dbdirect dbdirect-clean

default all: $(OBJS) drivers libHDB.a dbdirect

libHDB.a: $(OBJS) $(DRIVER_OBJS)
	$(AR) -rc $@ $^

drivers: $(DRIVER_OBJS)

$(DRIVER_OBJS):
	   $(MAKE) -C $(dir $@) $(notdir $@)

dbdirect:
	$(MAKE) -C dbdirect

.depend: $(SRC)
	$(GHC) -M -optdep-f -optdep.depend $(GHCFLAGS) $^

test: $(DRIVERS_TEST)

$(DRIVERS_TEST):
	$(MAKE) -C $(subst -test,,$@) test

clean: dbdirect-clean $(DRIVERS_CLEAN)
	-rm -f *.o *.hi *.a .depend .depend.bak

dbdirect-clean:
	$(MAKE) -C dbdirect clean

$(DRIVERS_CLEAN):
	$(MAKE) -C $(subst -clean,,$@) clean

include .depend

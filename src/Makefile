TOP_DIR = ..

include $(TOP_DIR)/rules.mk

OBJS = Database.o HaskellDB.o HDBRec.o HDBRecUtils.o Optimize.o \
       PrimQuery.o Query.o Sql.o FieldType.o

SRC = $(patsubst %.o,%.hs, $(OBJS))

DRIVERS_TEST = $(addsuffix -test, $(DRIVER_DIRS))
DRIVERS_CLEAN = $(addsuffix -clean, $(DRIVER_DIRS))
DRIVERS_OBJS = $(addsuffix /*.o, $(DRIVER_DIRS))

.PHONY: drivers $(DRIVER_DIRS) $(DRIVERS_OBJS) \
	dbdirect dbdirect-clean test $(DRIVERS_TEST) \
	$(DRIVERS_CLEAN)

all: $(OBJS) drivers libHShdb.a dbdirect

warn-all: all

libHShdb.a: $(OBJS) $(DRIVERS_OBJS)
	$(AR) -rc $@ $^

drivers: $(DRIVER_DIRS)

$(DRIVER_DIRS):
	$(MAKE) -C $@

dbdirect:
	$(MAKE) -C dbdirect

test: $(DRIVERS_TEST)

$(DRIVERS_TEST):
	$(MAKE) -C $(subst -test,,$@) test

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

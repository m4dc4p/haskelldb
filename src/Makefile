include config.mk
include rules.mk

OBJS = Database.o HaskellDB.o HDBRec.o HDBRecUtils.o Optimize.o PPrint.o \
       PrimQuery.o Query.o Sql.o FieldType.o

DRIVERS = hsql

SRC = $(patsubst %.o,%.hs, $(OBJS))
DRIVERS_CLEAN = $(addsuffix -clean, $(DRIVERS))

.PHONY: all clean dbdirect dbdirect-clean drivers $(DRIVERS) $(DRIVERS_CLEAN)

default all: libHDB.a drivers dbdirect


libHDB.a: $(OBJS)
	$(AR) -rc $@ $^

drivers: $(DRIVERS)

$(DRIVERS):
	   $(MAKE) -C $(addprefix drivers/, $@)
dbdirect:
	$(MAKE) -C dbdirect

.depend: $(SRC)
	$(GHC) -M -optdep-f -optdep.depend $(GHCFLAGS) $^

clean: dbdirect-clean $(DRIVERS_CLEAN)
	-rm -f *.o *.hi *.a .depend .depend.bak

dbdirect-clean:
	$(MAKE) -C dbdirect clean

$(DRIVERS_CLEAN):
	make -C $(addprefix drivers/, $(subst -clean,,$@)) clean

include .depend

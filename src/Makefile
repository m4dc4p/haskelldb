include config.mk
include rules.mk

OBJS = Database.o HaskellDB.o HDBRec.o HDBRecUtils.o Optimize.o PPrint.o \
       PrimQuery.o Query.o Sql.o FieldType.o

DRIVERS = hsql

SRC = $(patsubst %.o,%.hs, $(OBJS))

.PHONY: clean drivers

default all: libHDB.a drivers

libHDB.a: $(OBJS)
	$(AR) -rc $@ $^

drivers: 
	for d in $(addprefix drivers/, $(DRIVERS)); do \
	   $(MAKE) -C $$d; \
	done

.depend: $(SRC)
	$(GHC) -M -optdep-f -optdep.depend $(GHCFLAGS) $^

clean:
	-rm -f *.o *.hi *.a .depend .depend.bak
	for d in $(addprefix drivers/, $(DRIVERS)); do \
	   $(MAKE) -C $$d clean; \
	done

include .depend

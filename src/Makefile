HC=ghc
HC_SPEC_FLAGS=-fglasgow-exts -fallow-overlapping-instances
HC_LDFLAGS=
HC_INCLUDES=

HCFLAGS=$(HC_SPEC_FLAGS) $(HC_INCLUDES)

OBJS = Database.o HaskellDB.o HDBRec.o HDBRecUtils.o Optimize.o PPrint.o \
       PrimQuery.o Query.o Sql.o

.SUFFIXES: .hs .hi .o

.PHONY: clean odbc-driver

default all: libHDB.a

libHDB.a: $(OBJS)
	$(AR) -rc $@ $^

odbc-driver:
	cd drivers/odbc; $(MAKE)

.depend: *.hs
	$(HC) -M -optdep-f -optdep.depend $(HCFLAGS) *.hs

clean:
	-rm -f *.o *.hi *.a .depend .depend.bak

%.o: %.hs
	$(HC) $(HCFLAGS) -c $< -o $@

%.hi: %.o
	@\:

include .depend

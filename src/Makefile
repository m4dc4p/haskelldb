TOP_DIR = ..

SPEC_DIRS = DBSpec
DBDIRECT_DIRS = DBDirect

SUBDIRS = $(SPEC_DIRS) $(DBDIRECT_DIRS)

include $(TOP_DIR)/rules.mk

GHCFLAGS += -package-name haskelldb

HASKELLDB_MODULES = \
	  Database.HaskellDB.HDBRec \
	  Database.HaskellDB.FieldType \
	  Database.HaskellDB.PrimQuery \
	  Database.HaskellDB.Sql \
	  Database.HaskellDB.Query \
	  Database.HaskellDB.HDBRecUtils \
	  Database.HaskellDB.Optimize \
	  Database.HaskellDB.Database \
	  Database.HaskellDB.BoundedString \
	  Database.HaskellDB.BoundedList \
	  Database.HaskellDB.DBSpec \
	  Database.HaskellDB.GenericConnect

DATABASE_MODULES = Database.HaskellDB

MODULES = $(HASKELLDB_MODULES) $(DATABASE_MODULES)

DATABASE_SRC = \
      $(patsubst %, $(BUILD_DIR)/%.hs, $(subst .,/,$(DATABASE_MODULES)))

HASKELLDB_SRC = \
      $(patsubst %, $(BUILD_DIR)/%.hs, $(subst .,/,$(HASKELLDB_MODULES)))

SRC = $(HASKELLDB_SRC) $(DATABASE_SRC)

OBJS = $(patsubst %.hs, %.o, $(SRC))


ifeq "$(WITH_HSQL)" "yes"
DRIVERS_OBJS += $(BUILD_DIR)/Database/HaskellDB/HSQL/*.o
endif
ifeq "$(WITH_WX)" "yes"
DRIVERS_OBJS += $(BUILD_DIR)/Database/HaskellDB/WX.o
endif

SPEC_OBJS = $(BUILD_DIR)/Database/HaskellDB/DBSpec/*.o

ALL_COMPILERS = $(addprefix all-,$(COMPILERS))

.PHONY: $(ALL_COMPILERS) $(DRIVERS)

all: $(ALL_COMPILERS) $(SUBDIRS)

all-ghc: $(BUILD_DIR)/libHShdb.a

all-hugs: $(SRC) 

$(BUILD_DIR)/libHShdb.a: $(OBJS) $(SPEC_DIRS) $(DRIVERS)
	$(AR) -rc $@ $(filter %.o, $^) $(DRIVERS_OBJS) $(SPEC_OBJS)

$(BUILD_DIR)/Database/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/BoundedList.o: GHCFLAGS += -fallow-undecidable-instances

HSQL:
	make -C drivers/hsql

WX:
	make -C drivers/wxhaskell

# cleaning up

clean: 
	-rm -f *.o *.hi .depend .depend.bak GenericConnect.hs

# dependencies

.depend: $(SRC)

include .depend

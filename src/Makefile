TOP_DIR = ..

SUBDIRS = DBDirect

include $(TOP_DIR)/rules.mk

GHCFLAGS += -package-name haskelldb

DATABASE_MODULES = Database.HaskellDB

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
	  Database.HaskellDB.GenericConnect \
	  Database.HaskellDB.DBSpec \

DBSPEC_MODULES = \
	  Database.HaskellDB.DBSpec.DBInfo \
	  Database.HaskellDB.DBSpec.DBSpecToDatabase \
	  Database.HaskellDB.DBSpec.DBSpecToDBDirect \
	  Database.HaskellDB.DBSpec.DatabaseToDBSpec \
	  Database.HaskellDB.DBSpec.PPHelpers

HSQL_MODULES = $(addprefix Database.HaskellDB.HSQL., Common $(HSQL_DRIVERS))

WX_MODULES = Database.HaskellDB.WX

MODULES = $(DATABASE_MODULES) $(HASKELLDB_MODULES) $(DBSPEC_MODULES)

ifeq "$(WITH_HSQL)" "yes"
MODULES += $(HSQL_MODULES)
endif
ifeq "$(WITH_WX)" "yes"
MODULES += $(WX_MODULES)
endif

SRC = $(patsubst %, $(BUILD_DIR)/%.hs, $(subst .,/,$(MODULES)))

OBJS = $(patsubst %.hs, %.o, $(SRC))

ALL_COMPILERS = $(addprefix all-,$(COMPILERS))

.PHONY: $(ALL_COMPILERS)

all: $(ALL_COMPILERS) $(SUBDIRS)

all-ghc: $(BUILD_DIR)/libHShdb.a

all-hugs: $(SRC) 

$(BUILD_DIR)/libHShdb.a: $(OBJS)
	$(AR) -rc $@ $(filter %.o, $^)

$(BUILD_DIR)/Database/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/HSQL/%: drivers/hsql/%
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/WX.hs: drivers/wxhaskell/WX.hs
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/BoundedList.o: GHCFLAGS += -fallow-undecidable-instances


# cleaning up

clean: 
	-rm -f *.o *.hi .depend .depend.bak

# dependencies

.depend: $(SRC)

include .depend

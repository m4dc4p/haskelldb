TOP_DIR = ..

include $(TOP_DIR)/rules.mk

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

PROGRAMS = DBDirect

PROG_SRC = $(patsubst %, $(COMPILER_DIR)/%.hs, $(PROGRAMS))

SRC = $(patsubst %, $(COMPILER_DIR)/%.hs, $(subst .,/,$(MODULES)))

$(COMPILER_DIR)/Database/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(COMPILER_DIR)/Database/HaskellDB/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(COMPILER_DIR)/Database/HaskellDB/HSQL/%: drivers/hsql/%
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(COMPILER_DIR)/Database/HaskellDB/WX.hs: drivers/wxhaskell/WX.hs
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(COMPILER_DIR)/DBDirect.hs: DBDirect/DBDirect.hs
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

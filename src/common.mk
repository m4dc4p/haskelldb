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
	  Database.HaskellDB.DBLayout

DBSPEC_MODULES = \
	  Database.HaskellDB.DBSpec.DBInfo \
	  Database.HaskellDB.DBSpec.DBSpecToDatabase \
	  Database.HaskellDB.DBSpec.DBSpecToDBDirect \
	  Database.HaskellDB.DBSpec.DatabaseToDBSpec \
	  Database.HaskellDB.DBSpec.PPHelpers

MODULES = $(DATABASE_MODULES) $(HASKELLDB_MODULES) $(DBSPEC_MODULES)


ifeq "$(WITH_HSQL)" "yes"
MODULES += Database.HaskellDB.HSQL.Common

ifeq "$(WITH_HSQL_ODBC)" "yes"
MODULES += Database.HaskellDB.HSQL.ODBC
HSPP_FLAGS += -DWITH_HSQL_ODBC
endif

ifeq "$(WITH_HSQL_MYSQL)" "yes"
MODULES += Database.HaskellDB.HSQL.MySQL
HSPP_FLAGS += -DWITH_HSQL_MYSQL
endif

ifeq "$(WITH_HSQL_SQLITE)" "yes"
MODULES += Database.HaskellDB.HSQL.SQLite
HSPP_FLAGS += -DWITH_HSQL_SQLITE
endif

ifeq "$(WITH_HSQL_POSTGRESQL)" "yes"
MODULES += Database.HaskellDB.HSQL.PostgreSQL
HSPP_FLAGS += -DWITH_HSQL_POSTGRESQL
endif
endif

ifeq "$(WITH_WX)" "yes"
MODULES += Database.HaskellDB.WX
HSPP_FLAGS += -DWITH_WX
endif

PROGRAM_MODULES = DBDirect

PROGRAMS = $(addprefix $(COMPILER_DIR)/, $(PROGRAM_MODULES))

PROG_SRC = $(patsubst %, $(COMPILER_DIR)/%.hs, $(PROGRAM_MODULES))

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

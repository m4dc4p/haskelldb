	TOP_DIR = .

include $(TOP_DIR)/rules.mk

MODULES = Database.HaskellDB \
	  Database.HaskellDB.DriverAPI \
	  Database.HaskellDB.HDBRec \
	  Database.HaskellDB.FieldType \
	  Database.HaskellDB.PrimQuery \
	  Database.HaskellDB.Sql \
	  Database.HaskellDB.Query \
	  Database.HaskellDB.Optimize \
	  Database.HaskellDB.Database \
	  Database.HaskellDB.BoundedString \
	  Database.HaskellDB.BoundedList \
	  Database.HaskellDB.GenericConnect \
	  Database.HaskellDB.DBSpec \
	  Database.HaskellDB.DBLayout \
	  Database.HaskellDB.DBSpec.DBInfo \
	  Database.HaskellDB.DBSpec.DBSpecToDatabase \
	  Database.HaskellDB.DBSpec.DBSpecToDBDirect \
	  Database.HaskellDB.DBSpec.DatabaseToDBSpec \
	  Database.HaskellDB.DBSpec.PPHelpers

SRC_NODEP = $(patsubst %, $(COMPILER_DIR)/%.hs, $(subst .,/,$(MODULES)))

ifeq "$(WITH_HSQL)" "yes"
NEEDS_HSQL = Database.HaskellDB.HSQL.Common Database.HaskellDB.GenericConnect

ifeq "$(WITH_HSQL_ODBC)" "yes"
NEEDS_HSQL += Database.HaskellDB.HSQL.ODBC
HSPP_FLAGS += -DWITH_HSQL_ODBC
endif

ifeq "$(WITH_HSQL_MYSQL)" "yes"
NEEDS_HSQL += Database.HaskellDB.HSQL.MySQL
HSPP_FLAGS += -DWITH_HSQL_MYSQL
endif

ifeq "$(WITH_HSQL_SQLITE)" "yes"
NEEDS_HSQL += Database.HaskellDB.HSQL.SQLite
HSPP_FLAGS += -DWITH_HSQL_SQLITE
endif

ifeq "$(WITH_HSQL_POSTGRESQL)" "yes"
NEEDS_HSQL += Database.HaskellDB.HSQL.PostgreSQL
HSPP_FLAGS += -DWITH_HSQL_POSTGRESQL
endif

SRC_HSQL = $(patsubst %, $(COMPILER_DIR)/%.hs, $(subst .,/,$(NEEDS_HSQL)))
endif


ifeq "$(WITH_WX)" "yes"
NEEDS_WX = Database.HaskellDB.WX Database.HaskellDB.GenericConnect
HSPP_FLAGS += -DWITH_WX
SRC_WX = $(patsubst %, $(COMPILER_DIR)/%.hs, $(subst .,/,$(NEEDS_WX)))
endif

ifeq "$(WITH_GHC_PLUGINS)" "yes"
NEEDS_PLUGINS = Database.HaskellDB.DynConnect
SRC_PLUGINS = $(patsubst %, $(COMPILER_DIR)/%.hs, $(subst .,/,$(NEEDS_PLUGINS)))
endif

PROGRAM_MODULES = DBDirect

SRC = $(SRC_NODEP) $(SRC_HSQL) $(SRC_WX) $(SRC_PLUGINS)

PROGRAMS = $(addprefix $(COMPILER_DIR)/, $(PROGRAM_MODULES))

PROG_SRC = $(patsubst %, $(COMPILER_DIR)/%.hs, $(PROGRAM_MODULES))

$(COMPILER_DIR)/%.hs: src/%.hs
	mkdir -p $(dir $@)
	cp -f $< $@

$(COMPILER_DIR)/%.hs: src/%.pphs
	mkdir -p $(dir $@)
	$(HSPP) $(HSPP_FLAGS) $^ > $@

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

ifeq "$(WITH_GHC_PLUGINS)" "yes"
MODULES += Database.HaskellDB.DynConnect
endif

PROGRAM_MODULES = DBDirect

SRC = $(patsubst %, $(COMPILER_DIR)/%.hs, $(subst .,/,$(MODULES)))

PROGRAMS = $(addprefix $(COMPILER_DIR)/, $(PROGRAM_MODULES))

PROG_SRC = $(patsubst %, $(COMPILER_DIR)/%.hs, $(PROGRAM_MODULES))


$(COMPILER_DIR)/%.hs: src/%.hs
	mkdir -p $(dir $@)
	cp -f $< $@

$(COMPILER_DIR)/%.hs: src/%.pphs
	mkdir -p $(dir $@)
	$(HSPP) $(HSPP_FLAGS) $^ > $@

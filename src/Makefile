TOP_DIR = ..

DRIVER_DIRS = drivers/hsql

SUBDIRS = dbdirect $(DRIVER_DIRS)

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
	  Database.HaskellDB.Database

DATABASE_MODULES = Database.HaskellDB

MODULES = $(HASKELLDB_MODULES) $(DATABASE_MODULES)

DATABASE_SRC = \
      $(patsubst %, $(BUILD_DIR)/%.hs, $(subst .,/,$(DATABASE_MODULES)))

HASKELLDB_SRC = \
      $(patsubst %, $(BUILD_DIR)/%.hs, $(subst .,/,$(HASKELLDB_MODULES)))

SRC = $(HASKELLDB_SRC) $(DATABASE_SRC)

OBJS = $(patsubst %.hs, %.o, $(SRC))

DRIVERS_OBJS = $(patsubst %, $(BUILD_DIR)/Database/HaskellDB/%/*.o, $(DRIVERS))

ALL_COMPILERS = $(addprefix all-,$(COMPILERS))

.PHONY: $(ALL_COMPILERS)

all: $(ALL_COMPILERS) $(DRIVER_DIRS) dbdirect

all-ghc: $(BUILD_DIR)/libHShdb.a $(BUILD_DIR)/HShdb.o

all-hugs: $(SRC) 

$(BUILD_DIR)/libHShdb.a: $(OBJS) $(DRIVER_DIRS)
	$(AR) -rc $@ $(filter %.o, $^) $(DRIVERS_OBJS)

$(BUILD_DIR)/HShdb.o: $(BUILD_DIR)/libHShdb.a
	$(LD) -r --whole-archive -o $@ $^

$(HASKELLDB_SRC): $(notdir $(HASKELLDB_SRC))
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))
	echo Driver DIRS: $(DRIVER_DIRS)

$(DATABASE_SRC): $(notdir $(DATABASE_SRC))
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

# cleaning up

clean: 
	-rm -f .depend .depend.bak

# dependencies

.depend: $(SRC)

include .depend

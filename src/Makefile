TOP_DIR = ..

DRIVER_DIRS = drivers/hsql

SUBDIRS = DBSpec $(DRIVER_DIRS) DBDirect

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
	  Database.HaskellDB.BoundedList

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

all: $(ALL_COMPILERS) $(SUBDIRS)

all-ghc: $(BUILD_DIR)/libHShdb.a

all-hugs: $(SRC) 

$(BUILD_DIR)/libHShdb.a: $(OBJS) $(DRIVER_DIRS)
	$(AR) -rc $@ $(filter %.o, $^) $(DRIVERS_OBJS)

$(BUILD_DIR)/Database/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/%: %
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

$(BUILD_DIR)/Database/HaskellDB/BoundedList.o: GHCFLAGS += -fallow-undecidable-instances

# cleaning up

clean: 
	-rm -f *.o *.hi .depend .depend.bak

# dependencies

.depend: $(SRC)

include .depend

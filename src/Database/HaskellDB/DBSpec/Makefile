TOP_DIR = ../..

include $(TOP_DIR)/rules.mk

GHCFLAGS += -package-name haskelldb

SRC_HERE = DBInfo.hs \
	   DBSpecToDatabase.hs \
	   DBSpecToDBDirect.hs \
	   DatabaseToDBSpec.hs \
	   PPHelpers.hs

SRC = $(addprefix $(BUILD_DIR)/Database/HaskellDB/DBSpec/, $(SRC_HERE))
OBJS = $(patsubst %.hs,%.o, $(SRC))

ALL_COMPILERS = $(addprefix all-,$(COMPILERS))

.PHONY: $(ALL_COMPILERS)

all: $(ALL_COMPILERS)

all-ghc: $(OBJS)

all-hugs: $(SRC)

$(SRC): $(SRC_HERE)
	mkdir -p $(sort $(dir $@))
	cp -f $^ $(sort $(dir $@))

clean:
	-rm -f *.o *.hi .depend .depend.bak

.depend: $(SRC)

include .depend

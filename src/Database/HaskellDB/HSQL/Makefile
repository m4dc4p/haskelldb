TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

GHCFLAGS += -package-name haskelldb

SRC_HERE = Common.hs $(addsuffix .hs, $(HSQL_DRIVERS))

SRC = $(addprefix $(BUILD_DIR)/Database/HaskellDB/HSQL/, $(SRC_HERE))
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

TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

DRIVER_SRC = $(addsuffix .hs, $(HSQL_DRIVERS))
SRC = HSQL_driver.hs $(DRIVER_SRC)
OBJS = $(patsubst %.hs,%.o, $(SRC))

ALL_COMPILERS = $(addprefix all-,$(COMPILERS))

.PHONY: $(ALL_COMPILERS)

all: $(ALL_COMPILERS)

all-ghc: $(OBJS)

all-hugs:

.depend: $(SRC)

clean:
	-rm -f *.so *.o *.hi .depend .depend.bak

include .depend

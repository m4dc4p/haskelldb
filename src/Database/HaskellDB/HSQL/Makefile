TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

SRC = $(addsuffix .hs, $(HSQL_DRIVERS))
OBJS = $(patsubst %.hs,%.o, $(SRC))

.PHONY: drivers

all: drivers

drivers: $(OBJS)

.depend: $(SRC)

clean:
	-rm -f *.so *.o *.hi .depend .depend.bak

include .depend

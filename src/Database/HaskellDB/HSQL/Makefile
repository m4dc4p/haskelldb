TOP_DIR = ../../..

include $(TOP_DIR)/rules.mk

DRIVER_SRC = $(addsuffix .hs, $(HSQL_DRIVERS))
SRC = HSQL_driver.hs $(DRIVER_SRC)
OBJS = $(patsubst %.hs,%.o, $(SRC))

.PHONY: drivers

all: drivers

drivers: $(OBJS)

.depend: $(SRC)

clean:
	-rm -f *.so *.o *.hi .depend .depend.bak

include .depend

TOP_DIR = ..

include ../config.mk

PROG = hardcoded-layout-simple-query join-and-aggr html-users-list \
       big-test big-test-create-tables current-time date-types \
       insert-update-delete test-bounded transaction

.PHONY: all

all: $(PROG)

%: %.hs TestConnect.hs
	ghc --make -fcontext-stack30 -o $@ $<

%.hs: %.pphs
	$(TOP_DIR)/preprocess $(HSPP_FLAGS) $^ > $@

clean:
	-rm -f $(PROG) *.o *.hi 
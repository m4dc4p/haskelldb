TOP_DIR = ..

include $(TOP_DIR)/config.mk

PROG = hardcoded-layout-simple-query join-and-aggr html-users-list \
       big-test big-test-create-tables current-time date-types \
       insert-update-delete test-bounded transaction

.PHONY: all

all: $(PROG)

%.hs: %.pphs
	$(TOP_DIR)/preprocess $(HSPP_FLAGS) $^ > $@

%: %.hs
	ghc --make -fcontext-stack30 -o $@ $<

clean:
	-rm -f $(PROG) *.o *.hi 
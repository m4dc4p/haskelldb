TOP_DIR = ..

include $(TOP_DIR)/config.mk

PROG = hardcoded-layout-simple-query join-and-aggr html-users-list \
       current-time date-types \
       insert-update-delete test-bounded transaction lazy \
       higher-order big-test null-and-case default-auto-inc \
       join-without-restrict test-sum

.PHONY: all

all: $(PROG)

%: %.hs TestConnect.hs
	ghc --make -fcontext-stack30 -o $@ $<

clean:
	-rm -f $(PROG) *.o *.hi  
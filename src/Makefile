TOP_DIR = ..

include $(TOP_DIR)/rules.mk

ALL_COMPILERS = $(addprefix all-,$(COMPILERS))
CLEAN_COMPILERS = $(addprefix clean-,$(COMPILERS))

.PHONY: $(ALL_COMPILERS)

all: $(ALL_COMPILERS)

$(ALL_COMPILERS):
	$(MAKE) -f $(subst all-,Makefile., $@)

clean: $(CLEAN_COMPILERS)

$(CLEAN_COMPILERS):
	$(MAKE) -f $(subst clean-,Makefile., $@) clean

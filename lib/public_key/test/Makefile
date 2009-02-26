include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk


INCLUDES= -I. -I$(ERL_TOP)/lib/test_server/include/  -I ../include \

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	public_key_SUITE \
	pkits_SUITE

ERL_FILES= $(MODULES:%=%.erl)

HRL_FILES= 

TARGET_FILES= \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

SPEC_FILES = public_key.spec

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/public_key_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += $(INCLUDES)

EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)


clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(SPEC_FILES) $(ERL_FILES) $(HRL_FILES)$(RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)
release_docs_spec:



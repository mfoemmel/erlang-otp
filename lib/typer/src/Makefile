#=============================================================================
#
#    File:  lib/typer/src/Makefile
# Authors:  Kostis Sagonas
#
#=============================================================================

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TYPER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/typer-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES =  \
	typer \
	typer_generator \
	typer_info \
	typer_map \
	typer_options \
	typer_parse \
	typer_preprocess

YRL_FILES= typer_parse.yrl

HRL_FILES= typer.hrl
ERL_FILES= $(MODULES:%=%.erl)
INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
TARGET_FILES= $(INSTALL_FILES)
 
APP_FILE= typer.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= typer.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +warn_obsolete_guard

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core
	rm -f typer_parse.erl

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/typer_options.$(EMULATOR): typer_options.erl ../vsn.mk Makefile
	erlc -W $(ERL_COMPILE_FLAGS) -DVSN="\"v$(VSN)\"" -o$(EBIN) typer_options.erl

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

# ---------------------------------------------------------------------
# dependencies -- I wish they were somehow automatically generated
# ---------------------------------------------------------------------

$(EBIN)/typer.beam: typer.hrl
$(EBIN)/typer_generator.beam: typer.hrl
$(EBIN)/typer_info_collector.beam: typer.hrl
$(EBIN)/typer_options.beam: typer.hrl
$(EBIN)/typer_preprocess.beam: typer.hrl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(YRL_FILES) \
		$(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(INSTALL_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

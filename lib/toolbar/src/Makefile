# ----------------------------------------------------
# Copyright (C) 1997, Ericsson Telecommunications
# Author: Kenneth Lundin
#
# ----------------------------------------------------
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TOOLBAR_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/toolbar-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES=  \
	canvasbutton \
	toolbar \
	toolbar_graphics \
	toolbar_lib \
	toolbar_toolconfig

HRL_FILES= toolbar.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE = toolbar.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE = toolbar.appup
APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:


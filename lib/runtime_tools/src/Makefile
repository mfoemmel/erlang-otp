# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(RUNTIME_TOOLS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/runtime_tools-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	inviso_rt \
	inviso_rt_meta \
	inviso_rt_lib \
	inviso_as_lib \
	inviso_autostart \
	inviso_autostart_server \
	runtime_tools \
	runtime_tools_sup \
	dbg \
	observer_backend
HRL_FILES= ../include/observer_backend.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= runtime_tools.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= runtime_tools.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += +warn_unused_vars -I../include 

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
	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:








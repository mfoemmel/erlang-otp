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
VSN=$(DEBUGGER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/debugger-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	debugger \
	i \
	int \
	dbg_ets \
	dbg_icmd \
	dbg_ieval \
	dbg_iload \
	dbg_imeta \
	dbg_debugged \
	dbg_iserver \
	dbg_iserver_api \
	dbg_pretty \
	dbg_ui_aux \
	dbg_ui_break \
	dbg_ui_interpret \
	dbg_ui_mon \
	dbg_ui_mon_win \
	dbg_ui_trace \
	dbg_ui_trace_win \
	dbg_ui_view \
	dbg_ui_winman \
	dbg_ui_search \
	dbg_ui_recedit \
	dbg_idb \
	dbg_idb_server \
	dbg_ui_gotoline \
	dbg_ui_cache


HRL_FILES= \
	dbg_ui_data_struct.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

#TOOLBOX_FILES= debugger.tool debugger.gif
#TARGET_TOOLBOX_FILES= $(EBIN)/debugger.tool $(EBIN)/debugger.gif

APP_FILE =  debugger.app
APPUP_FILE = debugger.appup

APP_SRC = $(APP_FILE).src
APPUP_SRC = $(APPUP_FILE).src

APP_TARGET = $(EBIN)/$(APP_FILE)
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += 


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(TARGET_TOOLBOX_FILES)

clean:
	rm -f $(TARGET_FILES) $(TARGET_TOOLBOX_FILES)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

docs:

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(TOOLBOX_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(TARGET_TOOLBOX_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

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
VSN=$(GSB_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/gs-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES= browse_dialog browse_dialog_ui color_editor color_editor_ui draw \
	 file_dialog file_dialog_ui gen_db gen_file_dialog gsb gsb_db \
	 gsb_run gsb_ui hierarchy menu messages option save_soft_dialog \
	 save_soft_dialog_ui standard toolbox transfer 

HRL_FILES = draw.hrl gsb.hrl menu.hrl standard.hrl
GEN_HRL_FILES =
GTK_GENERIC_TARGET =
KEEP_AS_IS = .option .toolbox browse_dialog.rc color_editor.rc \
	file_dialog.rc gen_file_dialog.swr gen_file_dialog.trc \
	gsb.rc menutest.rc save_soft_dialog.rc gsb.gif gsb.tool

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(GEN_HRL_FILES) \
	$(GTK_GENERIC_TARGET) $(KEEP_AS_IS) $(APP_TARGET)

APP_FILE=
APP_SRC=
APP_TARGET=


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

doc:

clean:
	rm -f $(TARGET_FILES)
	rm -f core *~

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/.option: .option
	rm -f $@
	cp .option $@

$(EBIN)/.toolbox: .toolbox
	rm -f $@
	cp .toolbox $@

$(EBIN)/gsb.gif: gsb.gif
	rm -f $@
	cp gsb.gif $@

$(EBIN)/gsb.tool: gsb.tool
	rm -f $@
	cp gsb.tool $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(APP_SRC) $(ERL_FILES) $(HRL_FILES) $(GEN_HRL_FILES) \
		$(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(KEEP_AS_IS) $(RELSYSDIR)/ebin


release_doc_spec:

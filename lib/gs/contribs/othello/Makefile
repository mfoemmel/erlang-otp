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
include ../../vsn.mk
VSN=$(GS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/gs-$(VSN)/contribs

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
       othello \
       othello_adt \
       othello_board

HRL_FILES= 

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(TARGET_TOOLBOX_FILES)

TOOLNAME = othello

EXTRA_FILES=
TOOLBOX_FILES= $(TOOLNAME).tool $(TOOLNAME).gif
TARGET_TOOLBOX_FILES= $(TOOLBOX_FILES:%=$(EBIN)/%)
BITMAPS= priv/marker.bm priv/square.bm

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/$(TOOLNAME).gif: $(TOOLNAME).gif
	rm -f $@
	cp $(TOOLNAME).gif $@

$(EBIN)/$(TOOLNAME).tool: $(TOOLNAME).tool
	rm -f $@
	cp $(TOOLNAME).tool $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/othello
	$(INSTALL_DATA) $(ERL_FILES) $(EXTRA_FILES) $(RELSYSDIR)/othello
	$(INSTALL_DIR) $(RELSYSDIR)/othello/priv
	$(INSTALL_DATA) $(BITMAPS) $(TOOLBOX_FILES) $(RELSYSDIR)/othello/priv

release_docs_spec:


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

ifndef EBIN
EBIN = ../ebin
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(HIPE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/hipe-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = hipe_hash hipe_internal \
	 hipe_beam_vectors \
	hipe_pure_hash hipe_pure_ufind hipe_pure_vectors \
	hipe_timing hipe_ufind hipe_vectors \
	hipe_gb_hash hipe_dict_hash

HRL_FILES=
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

# APP_FILE= 
# APP_SRC= $(APP_FILE).src
# APP_TARGET= $(EBIN)/$(APP_FILE)
#
# APPUP_FILE= 
# APPUP_SRC= $(APPUP_FILE).src
# APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
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



# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:




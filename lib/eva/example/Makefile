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
VSN=$(EVA_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/eva-$(VSN)

# ----------------------------------------------------
# Common macros
# ----------------------------------------------------
EXAMPLE_FILES = README BOARD-MIB.mib board_test.erl simple_adaptation.erl
#	oms/src/oms.erl oms/ebin/oms.app


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt:

clean:

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/examples
	$(INSTALL_DIR) $(RELSYSDIR)/examples/oms
	$(INSTALL_DIR) $(RELSYSDIR)/examples/oms/src
	$(INSTALL_DIR) $(RELSYSDIR)/examples/oms/ebin
	$(INSTALL_DATA) $(EXAMPLE_FILES) $(RELSYSDIR)/examples
	$(INSTALL_DATA) oms/src/oms.erl $(RELSYSDIR)/examples/oms/src/oms.erl
	$(INSTALL_DATA) oms/ebin/oms.app $(RELSYSDIR)/examples/oms/ebin/oms.app


release_docs_spec:

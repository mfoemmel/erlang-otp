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
MODULES = hipe_finalize hipe_hot_cold hipe_rtl2sparc \
	  hipe_sparc hipe_sparc_assemble hipe_sparc_assert hipe_sparc_cfg \
	  hipe_sparc_liveness hipe_sparc_op hipe_sparc_registers \
	  hipe_sparc_size hipe_sparc_multimove \
	  hipe_sparc_saver hipe_sparc_linker \
	  hipe_sparc_ra_coalescing \
	  hipe_sparc_ra_graph_color \
	  hipe_sparc_ra_ls \
	  hipe_sparc_ra_ols \
	  hipe_sparc_ra_memory \
	  hipe_sparc_ra


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

$(EBIN)/hipe_sparc_cfg.beam: ../flow/cfg.inc
$(EBIN)/hipe_sparc_liveness.beam: ../flow/liveness.inc

$(EBIN)/hipe_finalize.beam: ../main/hipe.hrl
$(EBIN)/hipe_rtl2sparc.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_linker.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_multimove.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_coalescing.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_graph_color.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_ls.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_memory.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_saver.beam: ../main/hipe.hrl

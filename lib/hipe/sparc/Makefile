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

ifndef DOCS
DOCS = ../doc
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
DOC_MODULES = hipe_finalize hipe_hot_cold hipe_rtl2sparc \
	  hipe_sparc hipe_sparc_assemble hipe_sparc_assert hipe_sparc_cfg \
	  hipe_sparc_liveness hipe_sparc_op hipe_sparc_registers \
	  hipe_sparc_size hipe_sparc_multimove \
	  hipe_sparc_saver hipe_sparc_linker \
	  hipe_sparc_ra_coalescing hipe_sparc_ra_post_ls \
	  hipe_sparc_ra_cs hipe_sparc_ra_post_cs \
	  hipe_sparc_ra_graph_color \
	  hipe_sparc_ra_ls hipe_sparc_ra_postconditions \
	  hipe_sparc_ra_memory hipe_sparc_frame\
	  hipe_sparc_caller_saves hipe_sparc_stack_descriptors\
	  hipe_sparc_ra hipe_sparc_pp hipe_sparc_prop_env\
	  hipe_sparc_prop hipe_sparc_opt_frame \
	  hipe_sparc_ra_fp_naive hipe_sparc_ra_fp_ls \
	  hipe_sparc_ra_post_ls_fp
MODULES = $(DOC_MODULES) hipe_sparc_ebb

HRL_FILES=
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(DOC_MODULES:%=$(DOCS)/%.html)

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
ERL_COMPILE_FLAGS += +inline

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

docs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(DOCS)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s erlang halt

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

$(EBIN)/hipe_sparc_cfg.beam: ../flow/cfg.inc ../main/hipe.hrl
$(EBIN)/hipe_sparc_ebb.beam: ../flow/ebb.inc
$(EBIN)/hipe_sparc_liveness.beam: ../util/hipe_vector.hrl ../flow/liveness.inc

$(EBIN)/hipe_sparc_registers.beam: ../rtl/hipe_literals.hrl
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
$(EBIN)/hipe_sparc_saver.beam: ../main/hipe.hrl ../rtl/hipe_literals.hrl
$(EBIN)/hipe_sparc_pp.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_caller_saves.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_prop.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_prop_env.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_post_ls.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_assemble.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_fp_naive.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_frame.beam: ../main/hipe.hrl ../rtl/hipe_literals.hrl ../util/hipe_vector.hrl
$(EBIN)/hipe_sparc_ra_cs.beam: ../main/hipe.hrl ../util/hipe_vector.hrl
$(EBIN)/hipe_sparc_ra_post_cs.beam: ../main/hipe.hrl ../util/hipe_vector.hrl

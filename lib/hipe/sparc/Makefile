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
MODULES=hipe_finalize \
	hipe_rtl_to_sparc \
	hipe_sparc \
	hipe_sparc_assemble \
	hipe_sparc_caller_saves \
	hipe_sparc_cfg \
	hipe_sparc_finalize \
	hipe_sparc_frame \
	hipe_sparc_liveness \
	hipe_sparc_main \
	hipe_sparc_multimove \
	hipe_sparc_op \
	hipe_sparc_opt_frame \
	hipe_sparc_peephole \
	hipe_sparc_pp \
	hipe_sparc_prop \
	hipe_sparc_prop_env \
	hipe_sparc_ra \
	hipe_sparc_ra_fp_ls \
	hipe_sparc_ra_ls \
	hipe_sparc_ra_naive \
	hipe_sparc_ra_post_ls \
	hipe_sparc_ra_post_ls_fp \
	hipe_sparc_ra_postconditions \
	hipe_sparc_registers

HRL_FILES=
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

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
ERL_COMPILE_FLAGS += +warn_unused_import +warn_obsolete_guard

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

docs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(DOCS)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s init stop

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

$(EBIN)/hipe_finalize.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_rtl2sparc.beam: ../main/hipe.hrl ../rtl/hipe_rtl.hrl
$(EBIN)/hipe_sparc.beam: ../main/hipe.hrl hipe_sparc.hrl hipe_sparc_sdesc.hrl
$(EBIN)/hipe_sparc_assemble.beam: ../main/hipe.hrl hipe_sparc.hrl ../rtl/hipe_literals.hrl ../../kernel/src/hipe_ext_format.hrl
$(EBIN)/hipe_sparc_caller_saves.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_cfg.beam: ../flow/cfg.inc ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_finalize.beam: hipe_sparc.hrl
$(EBIN)/hipe_sparc_frame.beam: ../main/hipe.hrl hipe_sparc.hrl ../rtl/hipe_literals.hrl
$(EBIN)/hipe_sparc_liveness.beam: ../flow/liveness.inc
$(EBIN)/hipe_sparc_main.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_multimove.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_opt_frame.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_pp.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_prop.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_prop_env.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_ra_ls.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_memory.beam: ../main/hipe.hrl
$(EBIN)/hipe_sparc_ra_post_ls.beam: ../main/hipe.hrl hipe_sparc.hrl
$(EBIN)/hipe_sparc_registers.beam: ../rtl/hipe_literals.hrl

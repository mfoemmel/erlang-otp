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
DOC_MODULES = hipe_icode hipe_icode_cfg hipe_icode_liveness \
	hipe_icode_heap_test \
	hipe_icode_exceptions hipe_beam_to_icode hipe_icode_primops \
	hipe_icode_binary_pass \
	hipe_icode_ssa \
	hipe_icode_type hipe_icode_ssa_copy_prop hipe_icode_pp hipe_icode_fp \
	hipe_icode_ssa_const_prop \
	hipe_icode_split_arith hipe_icode_callgraph 

MODULES = $(DOC_MODULES) hipe_icode_ebb 

HRL_FILES=hipe_icode_type.hrl
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
ERL_COMPILE_FLAGS += +nowarn_shadow_vars

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
	$(INSTALL_DIR) $(RELSYSDIR)/icode
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/icode
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

$(EBIN)/hipe_icode.beam: hipe_icode.hrl ../main/hipe.hrl
$(EBIN)/hipe_icode_cfg.beam: ../flow/cfg.inc ../main/hipe.hrl
$(EBIN)/hipe_icode_ebb.beam: ../flow/ebb.inc
$(EBIN)/hipe_icode_liveness.beam: ../flow/liveness.inc
$(EBIN)/hipe_icode_heap_test.beam: ../main/hipe.hrl ../rtl/hipe_literals.hrl
$(EBIN)/hipe_icode_prop.beam: ../main/hipe.hrl
$(EBIN)/hipe_update_catches.beam: ../main/hipe.hrl
$(EBIN)/hipe_beam_to_icode.beam: hipe_icode.hrl ../main/hipe.hrl
$(EBIN)/hipe_icode_binary_pass.beam: ../rtl/hipe_literals.hrl

$(EBIN)/hipe_icode_ssa.beam: ../ssa/hipe_ssa.inc ../main/hipe.hrl ../ssa/hipe_ssa_liveness.inc
$(EBIN)/hipe_icode_ssa_const_prop.beam: ../main/hipe.hrl ../ssa/hipe_ssa_const_prop.inc
$(EBIN)/hipe_icode_ssa_copy_prop.beam: ../ssa/hipe_ssa_copy_prop.inc
$(EBIN)/hipe_icode_type.beam: hipe_icode_type.hrl

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
MODULES = hipe_rtl hipe_rtl_cfg hipe_rtl_cse hipe_rtl_ebb \
	  hipe_rtl_liveness hipe_rtl_prop \
	  hipe_icode2rtl hipe_tagscheme hipe_rtl_mk_switch \
	  hipe_rtl_primops hipe_rtl_gctests hipe_rtl_arch \
	  hipe_rtl_varmap hipe_rtl_exceptions hipe_rtl_guardops \
	  hipe_rtl_bs_ops hipe_rtl_inline_bs_ops \
	  hipe_rtl_arith hipe_rtl_arith_32 \
	  hipe_rtl_ssa_check


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
	rm -f hipe_literals.hrl
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


HIPE_MKLITERALS=$(ERL_TOP)/bin/$(TARGET)/hipe_mkliterals

$(HIPE_MKLITERALS):
	cd $(ERL_TOP)/erts/emulator \
	  && $(MAKE) generate ERL_TOP=$(ERL_TOP) TYPE=$(TYPE)

hipe_literals.hrl:	$(HIPE_MKLITERALS)
	$(HIPE_MKLITERALS) -e > hipe_literals.hrl

$(EBIN)/hipe_rtl.beam: hipe_rtl.hrl
$(EBIN)/hipe_rtl_arch.beam: hipe_literals.hrl
$(EBIN)/hipe_rtl_cfg.beam: ../flow/cfg.inc ../main/hipe.hrl
$(EBIN)/hipe_rtl_ebb.beam: ../flow/ebb.inc
$(EBIN)/hipe_rtl_liveness.beam: ../util/hipe_vector.hrl ../flow/liveness.inc
$(EBIN)/hipe_icode2rtl.beam: hipe_literals.hrl ../main/hipe.hrl
$(EBIN)/hipe_tagscheme.beam: hipe_literals.hrl
$(EBIN)/hipe_rtl_primops.beam: hipe_literals.hrl
$(EBIN)/hipe_rtl_guardops.beam: hipe_literals.hrl
$(EBIN)/hipe_rtl_prop.beam: ../main/hipe.hrl
$(EBIN)/hipe_rtl_arith_32.beam: ../main/hipe.hrl
$(EBIN)/hipe_rtl_ssa_check.beam: ../ssa/hipe_ssa_checker.inc

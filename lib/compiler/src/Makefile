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
VSN=$(COMPILER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/compiler-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES =  \
	compile \
	sys_pre_attributes \
	sys_pre_expand \
	sys_expand_pmod \
	v3_core \
	sys_core_fold \
	sys_core_inline \
	sys_core_dsetel \
	core_lib \
	core_scan \
	core_parse \
	core_lint \
	core_pp \
	v3_kernel \
	v3_kernel_pp \
	v3_life \
	v3_codegen \
	beam_block \
	beam_bool \
	beam_dead \
	beam_jump \
	beam_type \
	beam_clean \
	beam_flatten \
	beam_listing \
	beam_asm \
	beam_dict \
	beam_opcodes \
	beam_disasm \
	beam_validator \
        erl_bifs \
	cerl \
	cerl_clauses \
	cerl_inline \
	cerl_trees \
	rec_env

BEAM_H = $(wildcard ../priv/beam_h/*.h)

HRL_FILES= \
	core_parse.hrl \
	v3_kernel.hrl \
	v3_life.hrl
EXTRA_FILES= beam_opcodes.hrl

ERL_FILES= $(MODULES:%=%.erl)
INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
TARGET_FILES= $(INSTALL_FILES)
 
APP_FILE= compiler.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= compiler.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += +warn_unused_vars +inline
ERL_COMPILE_FLAGS += -I../../stdlib/include -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f beam_opcodes.erl beam_opcodes.hrl core_parse.erl
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

beam_opcodes.erl beam_opcodes.hrl: genop.tab
	$(PERL) $(ERL_TOP)/erts/emulator/utils/beam_makeops -compiler $<

$(EBIN)/beam_asm.beam: $(ESRC)/beam_asm.erl beam_opcodes.hrl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -DCOMPILER_VSN='"$(VSN)"' -o$(EBIN) $<

$(EBIN)/cerl_inline.beam: $(ESRC)/cerl_inline.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) +nowarn_shadow_vars -o$(EBIN) $<

$(EBIN)/beam_disasm.beam: beam_opcodes.hrl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(EXTRA_FILES) \
		$(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(INSTALL_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

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

#ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -W
#endif

EBIN = .

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../../vsn.mk
VSN=$(MNESIA_SESSION_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/mnesia_session-$(VSN)/examples

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
GEN_OUT_DIR= gen_files

PERSON_IDL = ../person.idl

IDL_FILES = \
	$(PERSON_IDL)

NORMAL_MODULES= \
	person_ex \
	setup

NORMAL_ERL_FILES = \
	$(NORMAL_MODULES:%=%.erl)

GEN_MODULES =  \
	oe_person \
	persons_data \
	persons_person

GEN_HRL_NAMES = \
	oe_person \
	persons

GEN_HRL_FILES = \
	$(GEN_HRL_NAMES:%=./$(GEN_OUT_DIR)/%.hrl)

GEN_FILES = \
	$(GEN_HRL_FILES) \
	$(GEN_MODULES:%=./$(GEN_OUT_DIR)/%.erl) 

TARGET_FILES= \
	$(GEN_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(NORMAL_MODULES:%=$(EBIN)/%.$(EMULATOR))


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ERL_TOP)/lib/mnesia_session/ebin

ERL_COMPILE_FLAGS += \
		-I ./$(GEN_OUT_DIR)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt: $(TARGET_FILES)

debug:
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_FILES)
	rm -f $(GEN_FILES)
	rm -f core

docs:

$(EBIN)/%.$(EMULATOR): $(GEN_OUT_DIR)/%.erl
	$(ERLC) -b$(EMULATOR) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<


$(GEN_FILES): $(PERSON_IDL)
	$(ERLC) $(ERL_FLAGS)  $(ERL_COMPILE_FLAGS) \
	+'{be,erl_genserv}' +'{outdir,"./gen_files"}' \
	$(PERSON_IDL)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

#$(APP_TARGET): $(APP_SRC)
#	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELSYSDIR)/person
	$(INSTALL_DIR) $(RELSYSDIR)/person/erl_session
	$(INSTALL_DIR) $(RELSYSDIR)/person/erl_session/gen_files
	$(INSTALL_DATA) $(IDL_FILES) $(RELSYSDIR)/person
	$(INSTALL_DATA) $(GEN_FILES) $(RELSYSDIR)/person/erl_session/gen_files
	$(INSTALL_DATA) $(TARGET_FILES) $(NORMAL_ERL_FILES) $(RELSYSDIR)/person/erl_session

release_docs_spec:



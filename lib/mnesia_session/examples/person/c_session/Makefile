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
PP_DIR= gen_person_files
MS_DIR= gen_session_files

PERSON_IDL = ../person.idl

IDL_FILES = \
	$(PERSON_IDL)

NORMAL_MODULES= \
	setup

NORMAL_ERL_FILES= \
	$(NORMAL_MODULES:%=%.erl)

GEN_MODULES =  \
	oe_person 


GEN_HRL_NAMES = \
	oe_person \
	persons

GEN_HRL_FILES = \
	$(GEN_HRL_NAMES:%=./$(PP_DIR)/%.hrl)

GEN_ERL_FILES = \
	$(GEN_HRL_FILES) \
	$(GEN_MODULES:%=./$(PP_DIR)/%.erl) 

GEN_PP_C_FILES = \
	$(PP_DIR)/oe_person.c \
	$(PP_DIR)/oe_code_persons_Sex.c \
	$(PP_DIR)/oe_code_persons_person_children.c \
	$(PP_DIR)/oe_code_persons_data.c \
	$(PP_DIR)/oe_code_persons_person.c

GEN_PP_O_FILES = \
	$(GEN_PP_C_FILES:%.c=%.o)

GEN_PP_H_FILES = \
	$(PP_DIR)/oe_person.h \
	$(PP_DIR)/persons.h

GEN_PP_FILES = \
	$(GEN_PP_C_FILES) \
	$(GEN_PP_H_FILES)

GEN_MS_C_FILES = \
	$(MS_DIR)/oe_code_mnesia_AccessMode.c \
	$(MS_DIR)/oe_code_mnesia_CheckpointDef.c \
	$(MS_DIR)/oe_code_mnesia_Checkpoints.c \
	$(MS_DIR)/oe_code_mnesia_AttrNames.c \
	$(MS_DIR)/oe_code_mnesia_Indices.c \
	$(MS_DIR)/oe_code_mnesia_KeyList.c \
	$(MS_DIR)/oe_code_mnesia_NodeList.c \
	$(MS_DIR)/oe_code_mnesia_Recordlist.c \
	$(MS_DIR)/oe_code_mnesia_SetOrBag.c \
	$(MS_DIR)/oe_code_mnesia_Status.c \
	$(MS_DIR)/oe_code_mnesia_Storage.c \
	$(MS_DIR)/oe_code_mnesia_SystemInfo.c \
	$(MS_DIR)/oe_code_mnesia_TableDef.c \
	$(MS_DIR)/oe_code_mnesia_TableInfo.c \
	$(MS_DIR)/oe_code_mnesia_TableList.c \
	$(MS_DIR)/oe_mnesia_session.c \
	$(MS_DIR)/mnesia_connector.c \
	$(MS_DIR)/mnesia_session.c

GEN_MS_O_FILES = \
	$(GEN_MS_C_FILES:%.c=%.o)

GEN_MS_H_FILES = \
	$(MS_DIR)/mnesia.h \
	$(MS_DIR)/mnesia_connector.h \
	$(MS_DIR)/mnesia_session.h \
	$(MS_DIR)/oe_mnesia_session.h 

GEN_MS_FILES = \
	$(GEN_MS_C_FILES) \
	$(GEN_MS_H_FILES)

COMMON_TARGET_FILES = \
	$(GEN_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(NORMAL_MODULES:%=$(EBIN)/%.$(EMULATOR))

TARGET_FILES= \
	$(COMMON_TARGET_FILES) \
	$(GEN_PP_FILES) \
	$(GEN_MS_FILES) 

DEV_TARGET_FILES= \
	$(COMMON_TARGET_FILES) \
	person_ex

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
#ERL_FLAGS +=  \
#		-pa $(ERL_TOP)/lib/ic/ebin \
#		-pa $(ERL_TOP)/lib/orber/ebin \
#		-pa $(ERL_TOP)/lib/mnesia_session/ebin

ERL_FLAGS += -pa $(ERL_TOP)/lib/mnesia_session/ebin


ERL_COMPILE_FLAGS += \
		-I ./$(PP_DIR) \
		-I $(ERL_TOP)/lib/ic/include


C_FLAGS += \
	$(DEBUG_C_FLAGS) \
	-I $(PP_DIR) \
	-I $(MS_DIR) 

# DEBUG CC = /opt/SUNWspro/bin/cc
# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt: $(TARGET_FILES)

debug:
	@${MAKE} TYPE=debug

dev: $(DEV_TARGET_FILES)
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_FILES)
	rm -f $(GEN_PP_O_FILES) $(GEN_MS_O_FILES) person_ex.o person_ex
	rm -f $(GEN_MS_FILES) $(GEN_PP_FILES)
	rm -f $(GEN_ERL_FILES)
	rm -f core

docs:

$(EBIN)/%.$(EMULATOR): $(PP_DIR)/%.erl
	$(ERLC) -b$(EMULATOR) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(GEN_ERL_FILES): $(PERSON_IDL)
	$(ERLC) $(ERL_FLAGS)  $(ERL_COMPILE_FLAGS) \
	+'{be,erl_genserv}' +'{outdir,"./$(PP_DIR)"}' \
	$(PERSON_IDL)

$(GEN_PP_FILES): $(PERSON_IDL)
	$(ERLC) $(ERL_FLAGS)  $(ERL_COMPILE_FLAGS) \
	+'{be,c_genserv}' +'{outdir,"./$(PP_DIR)"}' \
	$(PERSON_IDL)

$(GEN_MS_FILES): $(ERL_TOP)/lib/mnesia_session/src/mnesia_session.idl
	$(ERLC) $(ERL_FLAGS)  $(ERL_COMPILE_FLAGS) \
	+'{be,c_genserv}' +'{outdir,"./$(MS_DIR)"}' \
	$(ERL_TOP)/lib/mnesia_session/src/mnesia_session.idl

$(GEN_MS_O_FILES): $(GEN_MS_C_FILES)

$(GEN_PP_O_FILES): $(GEN_PP_C_FILES)

person_ex.o: person_ex.c $(GEN_PP_C_FILES)  $(GEN_MS_C_FILES)

%.o: %.c
	$(CC) -c $(C_FLAGS) $< -o $@

# Debug	-L /home/gandalf/gordon/erts/solaris-p5a-gdb/lib/\

PERSON_C_FLAGS= \
	-L $(ERL_ROOTDIR)/usr/lib \
	-lic -lerl_interface -lei -lsocket -lnsl

person_ex: person_ex.o $(GEN_PP_O_FILES) $(GEN_MS_O_FILES)
	$(CC) $(C_FLAGS)  $(GEN_PP_O_FILES) $(GEN_MS_O_FILES) -o $@ person_ex.c $(PERSON_C_FLAGS)

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
	$(INSTALL_DIR) $(RELSYSDIR)/person/c_session
	$(INSTALL_DIR) $(RELSYSDIR)/person/c_session/$(MS_DIR)
	$(INSTALL_DIR) $(RELSYSDIR)/person/c_session/$(PP_DIR)
	$(INSTALL_DATA) $(IDL_FILES) $(RELSYSDIR)/person
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/person/c_session/Makefile.skel
	$(INSTALL_DATA) $(COMMON_TARGET_FILES) person_ex.c $(NORMAL_ERL_FILES) $(RELSYSDIR)/person/c_session
	$(INSTALL_DATA) $(GEN_PP_FILES) $(GEN_ERL_FILES) $(RELSYSDIR)/person/c_session/$(PP_DIR)
	$(INSTALL_DATA) $(GEN_MS_FILES) $(RELSYSDIR)/person/c_session/$(MS_DIR)

release_docs_spec:


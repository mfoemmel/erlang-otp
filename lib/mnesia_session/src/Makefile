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

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(MNESIA_SESSION_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/mnesia_session-$(VSN)

INCLUDE_DIR= ../include

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
IDL_FILES = \
	mnesia_corba_session.idl \
	mnesia_session.idl

NORMAL_MODULES= \
	mnesia_connector_impl \
	mnesia_corba_connector_impl \
	mnesia_session_impl \
	mnesia_session_lib \
	mnesia_session_sup \
	mnesia_session_top_sup 

GEN_GS_MODULES = \
	mnesia_connector \
	mnesia_session \
	oe_mnesia_session

GEN_CORBA_MODULES =  \
	mnesia_CheckpointDef \
	mnesia_SystemInfo \
	mnesia_TableDef \
	mnesia_TableInfo \
	mnesia_corba_connector \
	mnesia_corba_session \
	oe_mnesia_corba_session 

GEN_MODULES= \
	$(GEN_GS_MODULES) \
	$(GEN_CORBA_MODULES) 

GEN_ERL_FILES = \
	$(GEN_MODULES:%=%.erl) 

MODULES = \
	$(GEN_MODULES) \
	$(NORMAL_MODULES) 

ERL_FILES= \
	$(MODULES:%=%.erl) 

NORMAL_HRL_FILES=

GEN_GS_HRL_FILES = \
	oe_mnesia_session.hrl \
	mnesia_session.hrl \
	mnesia_connector.hrl

GEN_CORBA_HRL_FILES = \
	oe_mnesia_corba_session.hrl \
	mnesia_corba_session.hrl \
	mnesia_corba_connector.hrl

GEN_CORBA_FILES = \
	$(GEN_CORBA_HRL_FILES) \
	$(GEN_CORBA_MODULES:%=%.erl) 

GEN_GS_FILES = \
	$(GEN_GS_HRL_FILES) \
	$(GEN_GS_MODULES:%=%.erl) 

GEN_HRL_FILES= \
	$(GEN_GS_HRL_FILES) \
	$(GEN_CORBA_HRL_FILES)

HRL_FILES= \
	$(NORMAL_HRL_FILES) \
	$(GEN_HRL_FILES)

NORMAL_TARGET_EBIN_FILES= \
	$(NORMAL_MODULES:%=$(EBIN)/%.$(EMULATOR))

TARGET_EBIN_FILES= \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(APP_TARGET) $(APPUP_TARGET)

APP_FILE= mnesia_session.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= mnesia_session.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)





# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
# Mnesia_session is included just to fool the code loader to find the
# include library used in include_lib(...)
ERL_FLAGS +=  \
		-pa $(ERL_TOP)/lib/mnesia_session/ebin \
		-I $(INCLUDE_DIR)


ERL_COMPILE_FLAGS += \
	+'{parse_transform, sys_pre_attributes}' \
	+'{attribute, insert, vsn, "mnesia_session_$(MNESIA_SESSION_VSN)"}'

ERL_ROOTDIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)

IC_DIR := $(shell echo 'io:format("~s~n",[code:lib_dir(ic)]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)

IDL_COMPILE_FLAGS += \
	-I ${IC_DIR}/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

# Because some .erl files get generated twice and compiled only the
# first time the .beam files are out of date when (if) we run make in
# this directory again. To avoid the recompiling that gets done in
# that case we put the generated files explicitly as a dependency
# which generates them all first.

opt: $(GEN_CORBA_FILES) $(GEN_GS_FILES) $(TARGET_EBIN_FILES)

debug:
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_EBIN_FILES)
	rm -f $(GEN_CORBA_FILES)
	rm -f $(GEN_GS_FILES)
	rm -f $(INCLUDE_DIR)/mnesia.hrl
	rm -f core

docs:

$(EBIN)/mnesia_session_impl.$(EMULATOR): $(INCLUDE_DIR)/mnesia.hrl

$(GEN_CORBA_FILES): mnesia_corba_session.idl 
	$(ERLC) $(ERL_FLAGS) $(IDL_COMPILE_FLAGS) \
	+'{preproc_cmd, "gcc -x c++ -E -D_CORBA_"}' \
	+'{{impl, "mnesia_corba_session"}, "mnesia_session_impl"}' \
	mnesia_corba_session.idl
	mv mnesia.hrl $(INCLUDE_DIR)

$(GEN_GS_FILES): mnesia_session.idl
	$(ERLC) $(ERL_FLAGS)  $(IDL_COMPILE_FLAGS) \
	+'{preproc_cmd, "gcc -x c++ -E"}' \
	+'{be, erl_genserv}' \
	mnesia_session.idl
	mv mnesia.hrl $(INCLUDE_DIR)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(TARGET_EBIN_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(IDL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(INCLUDE_DIR)/mnesia.hrl $(RELSYSDIR)/include

release_docs_spec:


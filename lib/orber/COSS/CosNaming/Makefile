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
EBIN=../../ebin

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(ORBER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/orber-$(VSN)
# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	CosNaming_NamingContext_impl \
        CosNaming_BindingIterator_impl \
	lname \
	lname_component \
	orber_cosnaming_utils

ERL_FILES = $(MODULES:%=%.erl) 
HRL_FILES = lname.hrl \
	orber_cosnaming.hrl
GEN_ERL_FILES =  \
	oe_cos_naming.erl \
	CosNaming_NamingContext.erl \
	CosNaming_BindingIterator.erl \
	CosNaming_NameComponent.erl \
	CosNaming_Binding.erl \
	CosNaming_NamingContext_NotFound.erl \
	CosNaming_NamingContext_AlreadyBound.erl \
	CosNaming_NamingContext_CannotProceed.erl \
	CosNaming_NamingContext_InvalidName.erl \
	CosNaming_NamingContext_NotEmpty.erl

GEN_HRL_FILES = \
	oe_cos_naming.hrl \
	CosNaming.hrl \
	CosNaming_NamingContext.hrl \
	CosNaming_BindingIterator.hrl

GEN_FILES = $(GEN_ERL_FILES) $(GEN_HRL_FILES)

TARGET_FILES = \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) 

IDL_FILE = cos_naming.idl

APP_FILE = 
#APP_SRC = $(APP_FILE).src
#APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin -pa $(ERL_TOP)/lib/ic/ebin
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/orber/include \
	+'{parse_transform, sys_pre_attributes}' \
	+'{attribute, insert, app_vsn, "orber_$(ORBER_VSN)"}' 

YRL_FLAGS = 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
opt: $(TARGET_FILES) $(APP_TARGET)

debug:
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(GEN_FILES): cos_naming.idl
	erlc $(ERL_FLAGS) +'{this, "CosNaming::NamingContext"}' cos_naming.idl

#	echo "ic:gen(cos_naming, [{this, \"CosNaming::NamingContext\"}]), halt()."| $(ERL) $(ERL_FLAGS)


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/COSS/CosNaming
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(IDL_FILE) $(RELSYSDIR)/COSS/CosNaming
	$(INSTALL_DATA) $(GEN_HRL_FILES) $(GEN_ERL_FILES) $(RELSYSDIR)/COSS/CosNaming


release_docs_spec:


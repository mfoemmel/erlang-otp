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

EBIN= ../../ebin

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

IDL_FILES = \
	stack.idl \
	InitialReferences.idl

GEN_ERL_MODULES =  \
	oe_stack \
	StackModule_Stack \
	StackModule_StackFactory 

MODULES= \
	StackModule_Stack_impl \
	StackModule_StackFactory_impl \
	stack_factory \
	stack_client

GEN_HRL_FILES = \
	oe_stack.hrl \
	StackModule.hrl \
	StackModule_Stack.hrl \
	StackModule_StackFactory.hrl

HRL_FILES= 

ERL_FILES= $(MODULES:%=%.erl)

JAVA_CLASSES = \
	StackClient

JAVA_FILES= $(JAVA_CLASSES:%=%.java)
CLASS_FILES= $(JAVA_CLASSES:%=%.class)

TARGET_FILES = \
	$(GEN_ERL_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

TIME_TEST_JAVA_FILES = \
	StackTimeClient.java

TIME_TEST_MODULES = \
	stack_time_client \
	stack_time_genclient

TEST_TARGET_FILES = $(TIME_TEST_MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ERL_TOP)/lib/orber/ebin -pa $(ERL_TOP)/lib/ic/ebin
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += -pa $(ERL_TOP)/lib/orber -I$(ERL_TOP)/lib/orber
YRL_FLAGS = 

JAVA_OPTIONS = -classpath ../../priv:/opt/local/pgm/OrbixWeb2.0.1

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES) $(GEN_ERL_MODULES:%=%.erl) $(GEN_HRL_FILES) $(CLASS_FILES)
	rm -f errs core *~

docs:

test: $(TEST_TARGET_FILES)


$(GEN_ERL_MODULES:%=%.erl) $(GEN_HRL_FILES): stack.idl
	erlc $(ERL_FLAGS) stack.idl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/examples/Stack
	$(INSTALL_DATA) $(ERL_FILES) $(JAVA_FILES) $(IDL_FILES) $(RELSYSDIR)/examples/Stack


release_docs_spec:



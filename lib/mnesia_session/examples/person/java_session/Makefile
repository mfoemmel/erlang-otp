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

JAR = jar

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
GEN_OUT_DIR= ./gen_files


PERSON_IDL = ../person.idl

MNESIA_SESSION_IDL = $(ERL_TOP)/lib/mnesia_session/src/mnesia_session.idl

ERLANG_INCLUDE = $(ERL_TOP)/lib/ic/include/

PERSON_JAR_FILE = person.jar

MNESIA_JAR_FILE = mnesia_session.jar

JAVA_FILES = \
	person_ex.java

IDL_FILES = \
	$(PERSON_IDL)

NORMAL_MODULES= \
	setup

NORMAL_ERL_FILES = \
	$(NORMAL_MODULES:%=%.erl)


GEN_MODULES =  \
	oe_person 


GEN_HRL_NAMES = \
	oe_person \
	persons

JAVA_CLIENT_NAME = \
	person_ex

GEN_PERSON_MODULE = \
	$(GEN_OUT_DIR)/persons

GEN_MNESIA_MODULE = \
	$(GEN_OUT_DIR)/mnesia

COMPILED_PERSON_MODULE = \
	persons

COMPILED_MNESIA_MODULE = \
	mnesia

COMPILED_JAVA_MODULES = \
	$(COMPILED_PERSON_MODULE)\
	$(COMPILED_MNESIA_MODULE)

GEN_PERSON_NAMES = \
	personPackage/childrenHolder \
	personPackage/childrenHelper \
	data \
	dataHelper \
	dataHolder \
	Sex \
	SexHelper \
	SexHolder \
	person \
	personHelper \
	personHolder  

GEN_MNESIA_NAMES = \
	AccessMode RecordlistHelper TableDefHolder \
	AccessModeHelper RecordlistHolder TableInfo \
	AccessModeHolder SetOrBag TableInfoHelper \
	AttrNamesHelper SetOrBagHelper TableInfoHolder \
	AttrNamesHolder SetOrBagHolder TableListHelper \
	CheckpointDef Status TableListHolder \
	CheckpointDefHelper StatusHelper _connectorImplBase \
	CheckpointDefHolder StatusHolder _connectorStub \
	CheckpointsHelper Storage _sessionImplBase \
	CheckpointsHolder StorageHelper _sessionStub \
	IndicesHelper StorageHolder connector \
	IndicesHolder SystemInfo connectorHelper \
	KeyListHelper SystemInfoHelper connectorHolder \
	KeyListHolder SystemInfoHolder session \
	NodeListHelper TableDef sessionHelper \
	NodeListHolder TableDefHelper sessionHolder 

ALL_JAVA_FILES = \
	$(GEN_PERSON_NAMES:%=$(GEN_PERSON_MODULE)/%.java) \
	$(GEN_MNESIA_NAMES:%=$(GEN_MNESIA_MODULE)/%.java) \
	$(JAVA_CLIENT_NAME:%=%.java)

GEN_HRL_FILES = \
	$(GEN_HRL_NAMES:%=./$(GEN_OUT_DIR)/%.hrl)


GEN_FILES = \
	$(GEN_HRL_FILES) \
	$(GEN_MODULES:%=./$(GEN_OUT_DIR)/%.erl) 

TARGET_FILES= \
	$(GEN_MODULES:%=$(EBIN)/%.$(EMULATOR)) \
	$(NORMAL_MODULES:%=$(EBIN)/%.$(EMULATOR)) 

# ----------------------------------------------------
# Java related programs and Flags
# ----------------------------------------------------

APP_DIR = $(ERL_TOP)/lib/mnesia_session

INCLUDE_DIR := $(APP_DIR)/include

IC_DIR := $(ERL_TOP)/lib/ic

JI_DIR := $(ERL_TOP)/lib/jinterface
APP_BUILD_DIR = $(shell (dirname $(shell (dirname $(shell (dirname $(shell pwd)))))))
 
IC_BUILD_DIR := $(shell echo 'io:format("~s~n",[code:lib_dir(ic)]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)
 
JI_BUILD_DIR := $(shell echo 'io:format("~s~n",[code:lib_dir(jinterface)]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)

APP_BUILD_DIR = $(shell (dirname $(shell (dirname $(shell (dirname $(shell pwd)))))))
 
IC_BUILD_DIR := $(shell echo 'io:format("~s~n",[code:lib_dir(ic)]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)
 
EI_BUILD_DIR := $(shell echo 'io:format("~s~n",[code:lib_dir(erl_interface)]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)

CLASSPATH = $(JI_DIR)/priv/OtpErlang.jar:$(JI_BUILD_DIR)/priv/OtpErlang.jar:$(IC_DIR)/priv/ic.jar:$(IC_BUILD_DIR)/priv/ic.jar

JAVA_FLAGS= -d .
JAVA_OPTIONS = 
JARFLAGS= -cvf


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS +=  -pa $(ERL_TOP)/lib/mnesia_session/ebin  \
	-pa $(ERL_TOP)/lib/ic/ebin

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt: $(TARGET_FILES)

debug:
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_FILES)
	rm -f $(GEN_FILES)
	rm -f $(GEN_GS_FILES)
	rm -rf $(GEN_PERSON_MODULE)
	rm -rf $(GEN_MNESIA_MODULE)
	rm -rf $(COMPILED_JAVA_MODULES)
	rm -rf person_ex.class
	rm -rf mnesia_session.jar
	rm -f core

docs:

$(EBIN)/%.$(EMULATOR): $(GEN_OUT_DIR)/%.erl
	$(ERLC) -b$(EMULATOR) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(GEN_FILES): $(PERSON_IDL)
	$(ERLC) $(ERL_COMPILE_FLAGS)\
	+'{be,erl_genserv}' +'{outdir,"./gen_files"}' $(PERSON_IDL)
	$(ERLC) $(ERL_COMPILE_FLAGS) \
	+'{be,java}' +'{outdir,"./gen_files"}' $(PERSON_IDL) 
	$(ERLC) $(ERL_COMPILE_FLAGS) \
	+'{be,java}' '+{preproc_flags,"-I$(ERLANG_INCLUDE)"}' +'{outdir,"./gen_files"}' \
	$(MNESIA_SESSION_IDL)
	CLASSPATH=$(CLASSPATH) $(JAVA) $(JAVA_FLAGS) $(ALL_JAVA_FILES)
	$(JAR) $(JARFLAGS) $(MNESIA_JAR_FILE) $(COMPILED_MNESIA_MODULE)

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
	$(INSTALL_DIR) $(RELSYSDIR)/person/java_session
	$(INSTALL_DATA) $(IDL_FILES) $(RELSYSDIR)/person
	$(INSTALL_DATA) person_ex.java $(RELSYSDIR)/person/java_session
	$(INSTALL_DATA) mnesia_session.jar $(RELSYSDIR)/person/java_session
	$(INSTALL_DATA) setup.erl $(RELSYSDIR)/person/java_session 
	$(INSTALL_DATA) setup.beam $(RELSYSDIR)/person/java_session 
	$(INSTALL_DATA) ReadMe $(RELSYSDIR)/person/java_session


release_docs_spec:








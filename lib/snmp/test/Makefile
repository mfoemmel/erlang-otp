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
# For an outline of how this all_SUITE_data stuff works, see the
# make file ../../ssl/test/Makefile.
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	snmp_test_lib \
	snmp_SUITE \
	snmp_app_test \
	snmp_appup_test \
	snmp_test \
	sa \
	klas3 \
	test1 \
	test2

SNMP_ROOT = ..
SNMP_SUITE = snmp_SUITE

ERL_FILES = $(MODULES:%=%.erl)

HRL_FILES = snmp_test_lib.hrl

SNMP_MIB_DIR = snmp_test_data

SNMP_BIN_TARGET_DIR = snmp_test_data

MIB_FILES = \
	OLD-SNMPEA-MIB.mib \
	OLD-SNMPEA-MIB-v2.mib \
	Klas1.mib \
	Klas1-v2.mib \
	Klas2.mib \
	Klas3.mib \
	Klas4.mib \
	SA-MIB.mib \
	EX1-MIB.mib \
	TestTrap.mib \
	TestTrapv2.mib \
	Test1.mib \
	Test2.mib

MIB_SOURCE  = $(MIB_FILES:%.mib=$(SNMP_MIB_DIR)/%.mib)

MIB_TARGETS = $(MIB_FILES:%.mib=$(SNMP_BIN_TARGET_DIR)/%.bin)
HRL_TARGETS = $(MIB_FILES:%.mib=$(SNMP_BIN_TARGET_DIR)/%.hrl)
ERL_TARGETS = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

TARGET_FILES = $(ERL_TARGETS) $(MIB_TARGETS) $(HRL_TARGETS)

SOURCE = $(ERL_FILES) $(HRL_FILES) 

SPECS = snmp.spec snmp.spec.vxworks 

# The script 'make_emakefile' only exist in R9 and later
# So, if it does not exist, then use the old method
# (compile the erl-files and install the beam-files)
EMAKEFILE  = Emakefile
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(SPECS) $(SOURCE) $(TARGET_FILES)
else
BUILDTARGET   = $(MIB_TARGETS) emakebuild
RELTEST_FILES = $(EMAKEFILE) $(SPECS) $(SOURCE) $(MIB_TARGETS) 
endif


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/snmp_test 


# ----------------------------------------------------
# SNMP FLAGS
# ----------------------------------------------------
ifeq ($(SNMP_DEBUG),)
  SNMP_DEBUG = d
endif
SNMP_FLAGS = 
ifeq ($(SNMP_DEBUG),e)
  SNMP_FLAGS += -Dsnmp_error
endif
ifeq ($(SNMP_DEBUG),l)
  SNMP_FLAGS += -Dsnmp_log
endif
ifeq ($(SNMP_DEBUG),d)
  SNMP_FLAGS += -Dsnmp_debug
endif

ifeq ($(SNMP_DESC),true)
  USE_DESCRIPTION = +'{description,true}'
endif

SNMP_MIB_FLAGS += \
	+'{group_check,false}' \
	$(USE_DESCRIPTION) \
	-I$(SNMP_BIN_TARGET_DIR)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EBIN = .

ERL_FLAGS += 
ERL_COMPILE_FLAGS += -I../src \
                     -I$(ERL_TOP)/lib/test_server/include \
                     -I../include \
                     $(SNMP_FLAGS)

ERL_SNMP_FLAGS = $(SNMP_MIB_FLAGS) \
                 -I../priv/mibs

$(SNMP_BIN_TARGET_DIR)/%.bin: $(SNMP_MIB_DIR)/%.mib
	$(ERLC) $(ERL_SNMP_FLAGS) -o $(SNMP_MIB_DIR) $<

# $(SNMP_BIN_TARGET_DIR)/%.bin: $(SNMP_MIB_DIR)/%.mib
# 	$(ERLC) -pa $(SNMP_TOOLKIT)/ebin $(SNMP_MIB_FLAGS) \
#                 -I $(SNMP_TOOLKIT)/priv/mibs -o $(SNMP_MIB_DIR) $<


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(BUILDTARGET)

.PHONY: emakebuild

emakebuild: $(EMAKEFILE)

targets: mib $(EMAKEFILE)
	erl $(ERL_FLAGS) -make

old_targets: $(TARGET_FILES)

$(EMAKEFILE):  Makefile
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' > $(EMAKEFILE)
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) >> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES) $(GEN_FILES)
	rm -f core

docs:

appup: snmp_appup_mgr.$(EMULATOR)

mib: $(MIB_TARGETS)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) $(RELTEST_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	tar cf - snmp_test_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:


info:
	@echo "SNMP_DEBUG        = $(SNMP_DEBUG)"
	@echo "SNMP_FLAGS        = $(SNMP_FLAGS)"
	@echo ""
	@echo "SNMP_MIB_DIR      = $(SNMP_MIB_DIR)"
	@echo "MIB_SOURCE        = $(MIB_SOURCE)"
	@echo "MIB_TARGETS       = $(MIB_TARGETS)"
	@echo "SNMP_MIB_FLAGS    = $(SNMP_MIB_FLAGS)"
	@echo ""
	@echo "ERL_FLAGS         = $(ERL_FLAGS)"
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
	@echo ""
	@echo "RELSYSDIR         = $(RELSYSDIR)"
	@echo ""
	@echo "SOURCE            = $(SOURCE)"
	@echo ""
	@echo "TARGET_FILES      = $(TARGET_FILES)"
	@echo ""
	@echo "EMAKEFILE         = $(EMAKEFILE)"
	@echo "MAKE_EMAKE        = $(MAKE_EMAKE)"
	@echo "BUILDTARGET       = $(BUILDTARGET)"
	@echo "RELTEST_FILES     = $(RELTEST_FILES)"
	@echo ""

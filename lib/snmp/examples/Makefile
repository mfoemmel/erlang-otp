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
VSN=$(SNMPEA_VSN)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifneq ($(MIBS_VERBOSITY),) 
SNMP_FLAGS += +'{verbosity,$(MIBS_VERBOSITY)}'
endif

ERL_FLAGS += 
ERL_COMPILE_FLAGS += -I../include \
                     -I$(ERL_TOP)/lib/stdlib \
                     $(SNMP_FLAGS)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/snmp-$(VSN)

# ----------------------------------------------------
# Common macros
# ----------------------------------------------------
BUILD_MIBS = EX1-MIB
TARGET_FILES= $(BUILD_MIBS:%=%.bin)

EXAMPLE_FILES = EX1-MIB.funcs snmp_mgr_tests.erl EX1-MIB.mib ex1.erl

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt:

clean:

docs:

build: $(TARGET_FILES)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/examples
	$(INSTALL_DATA) $(EXAMPLE_FILES) $(RELSYSDIR)/examples

release_docs_spec:

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
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/snmp-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

# NOTE: The MIB compiler is compiled from the directory ../mc_dummy;
# thus it is not included here!

MODULES=  \
	snmp_acm \
	snmp_agent \
	snmp_agent_sup \
	snmp_app \
	snmp_community_mib \
	snmp_conf \
	snmp_config \
	snmp_error \
	snmp_framework_mib \
	snmp_generic \
	snmp_generic_mnesia \
	snmp_index \
	snmp_local_db \
	snmp_log \
	snmp_mgr \
	snmp_mgr_misc \
	snmp_mib \
	snmp_mib_data \
	snmp_misc_sup \
	snmp_mpd \
	snmp_net_if \
	snmp_note_store \
	snmp_notification_mib \
	snmp_pdus \
	snmp_pets \
	snmp_set \
	snmp_set_lib \
	snmp_shadow_table \
	snmp_standard_mib \
	snmp_supervisor \
	snmp_svbl \
	snmp_symbolic_store \
	snmp_target_mib \
	snmp_trap \
	snmp_user_based_sm_mib \
	snmp_usm \
	snmp_vacm \
        snmp_verbosity \
	snmp_view_based_acm_mib

ERL_FILES = $(MODULES:%=%.erl)

HRLS =  snmp_verbosity \
	snmp_compile \
        snmp_debug  \
	snmp_vacm

HRL_FILES = $(HRLS:%=%.hrl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

EXT_HRL_FILES = snmp_types.hrl

APP_FILE = snmp.app
APPUP_FILE = snmp.appup

APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_SRC = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# SNMP FLAGS
# ----------------------------------------------------
ifeq ($(SNMP_DEFAULT_VERBOSITY),)
  SNMP_FLAGS = -Ddefault_verbosity=silence
else
  SNMP_FLAGS = -Ddefault_verbosity=$(SNMP_DEFAULT_VERBOSITY)
endif

#ifeq ($(SNMP_DEBUG),)
#  SNMP_DEBUG = d
#endif

ifeq ($(SNMP_DEBUG),d)
  SNMP_FLAGS += -Dsnmp_debug
endif


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -I../include \
                     -I$(ERL_TOP)/lib/stdlib \
                     $(SNMP_FLAGS)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)


clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f core *~

docs:


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
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) \
		$(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(EXT_HRL_FILES) $(RELSYSDIR)/include

release_docs_spec:


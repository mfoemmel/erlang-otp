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

ROOTDIR = $(ERL_TOP)/erts

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

ifeq ($(findstring win32,$(TARGET)),win32)

# This is a temporary fix until start_sasl is moved within the sasl app
# and each application can specify it's actions at install time (in Erlang).
INSTALL_SCRIPTS = \
	start_clean.script \
	start_clean.boot \
	start_sasl.boot \
	start_sasl.script
else
ifeq ($(findstring vxworks,$(TARGET)),vxworks)

INSTALL_SCRIPTS = \
	start_clean.script \
	start_clean.boot \
	start_sasl.boot \
	start_sasl.script
else

INSTALL_SCRIPTS = \
	start_clean.script \
	start_clean.boot
endif
endif 

REL_SCRIPTS = \
	start_clean.rel \
	start_sasl.rel \
	start_all_example.rel

SASL_PATH = -pa $(ROOTDIR)/lib/sasl/ebin 
ELIBROOT = ../..
ELIBDIR = $(ELIBROOT)/lib
SCRIPT_PATH = -I$(ELIBDIR)/kernel/ebin -I$(ELIBDIR)/stdlib/ebin -I$(ELIBDIR)/sasl/ebin
SCRIPT_FLAGS = 

##############################################################################
# Get version numbers from the VSN files

LIBPATH= ../../lib

# VSN & SYSTEM_VSN
include ../vsn.mk
include $(LIBPATH)/kernel/vsn.mk
include $(LIBPATH)/stdlib/vsn.mk
-include $(LIBPATH)/sasl/vsn.mk
-include $(LIBPATH)/os_mon/vsn.mk
-include $(LIBPATH)/mnesia/vsn.mk
-include $(LIBPATH)/snmp/vsn.mk
-include $(LIBPATH)/inets/vsn.mk

##############################################################################

debug opt instr script: rel $(INSTALL_SCRIPTS)

rel:	$(REL_SCRIPTS)

start_clean.script start_clean.boot:	start_clean.rel
	$(ERLC) $(SASL_FLAGS) $(SCRIPT_FLAGS) $(SASL_PATH) $(SCRIPT_PATH) $<

start_sasl.script start_sasl.boot:	start_sasl.rel
	$(ERLC) $(SASL_FLAGS) $(SCRIPT_FLAGS) $(SASL_PATH) $(SCRIPT_PATH) $<

start_clean.rel: start_clean.rel.src \
			../vsn.mk \
			$(LIBPATH)/kernel/vsn.mk \
			$(LIBPATH)/stdlib/vsn.mk
	sed -e 's;%SYS_VSN%;$(SYSTEM_VSN);' \
            -e 's;%ERTS_VSN%;$(VSN);' \
            -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
            -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
          start_clean.rel.src > start_clean.rel

start_sasl.rel:	start_sasl.rel.src \
			../vsn.mk \
			$(LIBPATH)/kernel/vsn.mk \
			$(LIBPATH)/stdlib/vsn.mk \
			$(LIBPATH)/sasl/vsn.mk
	sed -e 's;%SYS_VSN%;$(SYSTEM_VSN);' \
            -e 's;%ERTS_VSN%;$(VSN);' \
            -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
            -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
            -e 's;%SASL_VSN%;$(SASL_VSN);' \
          start_sasl.rel.src > start_sasl.rel

start_all_example.rel:	start_all_example.rel.src \
			../vsn.mk \
			$(LIBPATH)/kernel/vsn.mk \
			$(LIBPATH)/stdlib/vsn.mk \
			$(LIBPATH)/sasl/vsn.mk \
			$(LIBPATH)/os_mon/vsn.mk \
			$(LIBPATH)/mnesia/vsn.mk \
			$(LIBPATH)/snmp/vsn.mk \
			$(LIBPATH)/inets/vsn.mk
	sed -e 's;%SYS_VSN%;$(SYSTEM_VSN);' \
            -e 's;%ERTS_VSN%;$(VSN);' \
            -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
            -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
            -e 's;%SASL_VSN%;$(SASL_VSN);' \
            -e 's;%OS_MON_VSN%;$(OS_MON_VSN);' \
            -e 's;%MNESIA_VSN%;$(MNESIA_VSN);' \
            -e 's;%SNMP_VSN%;$(SNMPEA_VSN);' \
            -e 's;%INETS_VSN%;$(INETS_VSN);' \
          start_all_example.rel.src > start_all_example.rel

## Special target used from $(ERL_TOP)/erts/Makefile.
$(ERL_TOP)/bin/start.script:
	$(ERLC) $(SASL_PATH) \
	$(SCRIPT_PATH) $(SCRIPT_FLAGS) +otp_build -o $@ start_clean.rel

$(ERL_TOP)/bin/start_sasl.script:
	$(ERLC) $(SASL_PATH) \
	$(SCRIPT_PATH) $(SCRIPT_FLAGS) +otp_build -o $@ start_sasl.rel

clean:
	$(RM) $(REL_SCRIPTS) $(INSTALL_SCRIPTS)

docs:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: script
	$(INSTALL_DIR) $(RELEASE_PATH)/releases/$(SYSTEM_VSN)
	$(INSTALL_DATA) $(INSTALL_SCRIPTS) $(REL_SCRIPTS) \
	   $(RELEASE_PATH)/releases/$(SYSTEM_VSN)
	$(INSTALL_DATA) start_clean.script $(RELEASE_PATH)/releases/$(SYSTEM_VSN)/start.script
	$(INSTALL_DATA) start_clean.boot $(RELEASE_PATH)/releases/$(SYSTEM_VSN)/start.boot


release_docs_spec:

TRUE:


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
include vsn.mk

# ----------------------------------------------------------------------

ERTSDIRS = emulator doc/src
ERTSDIRS += etc epmd
ifeq ($(NO_START_SCRIPTS),)
ERTSDIRS += start_scripts
endif

BUILD_SHARED_EMU=true
ifeq ($(TARGET),win32)
	BUILD_SHARED_EMU=false
else
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
	BUILD_SHARED_EMU=false
else
ifeq ($(findstring ose,$(TARGET)),ose)
	BUILD_SHARED_EMU=false
else
	BUILD_SHARED_EMU=true
endif
endif
endif

all: shared opt

debug opt docs clean:
	@for d in $(ERTSDIRS); do \
		 if test -d $$d; then ( cd $$d && $(MAKE) $@ )fi ; \
	 done

# ----------------------------------------------------------------------
# These are "convenience targets", provided as shortcuts for developers
# - don't use them in scripts or assume they will always stay like this!
#

shared:
ifeq ($(BUILD_SHARED_EMU),true)
	( cd emulator && $(MAKE) $@ )
else
	@echo '*** Omitted build of shared heap emulator'
	@echo '*** since target is $(TARGET)'
endif

# Make erl script and erlc in $(ERL_TOP)/bin which runs the compiled version
# Note that erlc is not a script and requires extra handling on cygwin.
# also note that this file is not created by autoconf, that's why @EXEEXT@
# is not used.

# The copying of beam.dll should be removed when the beam dll need no longer be
# in the same directory...
local_setup:
	@cd start_scripts && $(MAKE)
	@rm -f $(ERL_TOP)/bin/erl $(ERL_TOP)/bin/erlc $(ERL_TOP)/bin/cerl \
		$(ERL_TOP)/bin/erl.exe $(ERL_TOP)/bin/erlc.exe \
		$(ERL_TOP)/bin/start*.boot $(ERL_TOP)/bin/start*.script
	@if [ "X$(TARGET)" = "Xwin32" ]; then \
		cp $(ERL_TOP)/bin/$(TARGET)/erlc.exe $(ERL_TOP)/bin/erlc.exe; \
		cp $(ERL_TOP)/bin/$(TARGET)/erl.exe $(ERL_TOP)/bin/erl.exe; \
		cp $(ERL_TOP)/bin/$(TARGET)/werl.exe $(ERL_TOP)/bin/werl.exe; \
	 	chmod 755 $(ERL_TOP)/bin/erl.exe $(ERL_TOP)/bin/erlc.exe \
			 $(ERL_TOP)/bin/werl.exe; \
		$(ERL_TOP)/erts/etc/win32/cygwin_tools/make_local_ini.sh \
			$(ERL_TOP); \
	else \
	sed	-e "s;%FINAL_ROOTDIR%;$(ERL_TOP);"   \
		-e "s;erts-.*/bin;bin/$(TARGET);"    \
		-e "s;EMU=.*;EMU=beam$(TYPEMARKER);" \
	        $(ERL_TOP)/erts/etc/unix/erl.src.src > $(ERL_TOP)/bin/erl; \
	sed	-e "s;%SRC_ROOTDIR%;$(ERL_TOP);"   \
		-e "s;%TARGET%;$(TARGET);"    \
		-e "s;%VSN%;$(VSN);"    \
	        $(ERL_TOP)/erts/etc/unix/cerl.src > $(ERL_TOP)/bin/cerl; \
		cp $(ERL_TOP)/bin/$(TARGET)/erlc $(ERL_TOP)/bin/erlc; \
	 	chmod 755 $(ERL_TOP)/bin/erl $(ERL_TOP)/bin/erlc \
			$(ERL_TOP)/bin/cerl; \
	fi
	@cd start_scripts && $(MAKE) $(ERL_TOP)/bin/start.script \
		$(ERL_TOP)/bin/start_sasl.script

# Run the configure script
configure:
	@set -e ; cd autoconf && $(MAKE)

# Remake the makefiles, if you already have configured but you have edited
# a "Makefile.in".
makefiles:
	@set -e ; cd autoconf && $(MAKE) $@

# ----------------------------------------------------------------------
# Release targets
#

release release_docs:
ifeq ($(BUILD_SHARED_EMU),true)
	@if test $@ = release; then ( cd emulator && $(MAKE) $@ TYPE=shared) fi
else
	@if test $@ = release; then ( $(MAKE) shared ) fi
endif
	@for d in $(ERTSDIRS); do \
		 if test -d $$d; then ( cd $$d && $(MAKE) $@ )fi ; \
	 done

# ----------------------------------------------------------------------

.PHONY: debug opt docs clean local_setup configure release \
	release_docs run_test_cases shared

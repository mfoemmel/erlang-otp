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

# ----------------------------------------------------------------------

ifneq ($(TARGET),win32)
ERTSDIRS = emulator doc/src
endif
ERTSDIRS += etc epmd
ifeq ($(NO_START_SCRIPTS),)
ERTSDIRS += start_scripts
endif


debug opt docs clean:
	@for d in $(ERTSDIRS); do \
		 if test -d $$d; then ( cd $$d && $(MAKE) $@ )fi ; \
	 done

# ----------------------------------------------------------------------
# These are "convenience targets", provided as shortcuts for developers
# - don't use them in scripts or assume they will always stay like this!
#

# Make erl and erlc scripts in $(ERL_TOP)/bin which runs the compiled version
local_setup:
ifneq ($(TARGET),win32)
	@cd start_scripts && $(MAKE)
	@rm -f $(ERL_TOP)/bin/erl $(ERL_TOP)/bin/erlc \
		$(ERL_TOP)/bin/start*.boot $(ERL_TOP)/bin/start*.script
	@sed	-e "s;%FINAL_ROOTDIR%;$(ERL_TOP);"   \
		-e "s;erts-.*/bin;bin/$(TARGET);"    \
		-e "s;EMU=.*;EMU=beam$(TYPEMARKER);" \
	        $(ERL_TOP)/erts/etc/unix/erl.src.src > $(ERL_TOP)/bin/erl
	@cp $(ERL_TOP)/bin/$(TARGET)/erlc $(ERL_TOP)/bin/erlc
	@chmod 755 $(ERL_TOP)/bin/erl $(ERL_TOP)/bin/erlc
	@cd start_scripts && $(MAKE) $(ERL_TOP)/bin/start.script \
		$(ERL_TOP)/bin/start_sasl.script
else
	@echo Nothing to do for "'"$@"'" on $(TARGET)
endif

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
# For release, you are supposed to make an  instrumented version of
# the emulator as well
#	@set -e ; cd emulator && $(MAKE) TYPE=instr $@

release release_docs:
	@for d in $(ERTSDIRS); do \
		 if test -d $$d; then ( cd $$d && $(MAKE) $@ )fi ; \
	 done

# ----------------------------------------------------------------------

.PHONY: debug opt docs clean local_setup configure release \
	release_docs run_test_cases

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

include ../epmd.mk

EBIN = .

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= epmd_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELEPMDDIR   = $(RELEASE_PATH)/epmd_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include \
                     -I$(ERL_TOP)/lib/kernel/src/         \
                     $(EPMD_FLAGS)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:

release_tests_spec: opt
	$(INSTALL_DIR) $(RELEPMDDIR)
	$(INSTALL_DATA) epmd.spec epmd.spec.vxworks $(ERL_FILES) \
		$(TARGET_FILES) $(RELEPMDDIR)
	chmod -f -R u+w $(RELEPMDDIR)

release_docs_spec:






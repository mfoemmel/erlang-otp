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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------

include ../vsn.mk
VSN=$(SSH_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------

RELSYSDIR = $(RELEASE_PATH)/ssh_test

ifeq ($(findstring win32,$(TARGET)),win32)

MAKEFILE_SRC = Makefile.win32.src

else

MAKEFILE_SRC = Makefile.src

endif

ifeq ($(TT_DIR),)
TT_DIR = /tmp
endif

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

HRL_FILES = ssh_test_lib.hrl

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(USE_SSH_TEST_CODE),true)
ERL_COMPILE_FLAGS += -DSSH_TEST_CODE
endif

ifeq ($(USE_SSH_HIPE),true)
ERL_COMPILE_FLAGS += +native
endif

ERL_FLAGS += \
           -pa $(ERL_TOP)/lib/ssh/ebin

ERL_COMPILE_FLAGS += \
           -pa $(ERL_TOP)/lib/test_server/ebin \
           -I$(ERL_TOP)/lib/test_server/include

ERL_PATH = \
	-pa ../../ssh/ebin

ifndef SUITE
SUITE = ssh_SUITE
endif

ifeq ($(NO_EHALT),true)
EHALT =
else
EHALT = -s init stop
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info:
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
	@echo ""

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

make: $(TARGET_FILES)

app: make
	erl -noshell -sname ssh_app $(ERL_PATH) \
            -s ssh_test_lib t ssh_app_test \
            -s erlang halt
test: app

# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  $(RELSYSDIR)
	$(INSTALL_DATA) $(TEST_SPEC_FILE) $(HRL_FILES) $(ERL_FILES) \
                        $(RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)


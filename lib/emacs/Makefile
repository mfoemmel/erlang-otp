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
include ../tools/vsn.mk
VSN=$(TOOLS_VSN)


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/tools-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	tags

MAN_FILES= \
	tags.3

EL_FILES= \
	erlang-start.el \
	erlang.el \
	erlang.elc

README_FILES= README

HRL_FILES= 

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

APP_FILES = 
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)
# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
# The Erlang code is installed in the tools application !!!!!
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELEASE_PATH)/misc/emacs
	$(INSTALL_DATA) $(EL_FILES) $(README_FILES) $(RELEASE_PATH)/misc/emacs

release_docs_spec: docs
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN_FILES) $(RELEASE_PATH)/man/man3


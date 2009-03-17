#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
#
#
include $(ERL_TOP)/make/target.mk

JAVA_DEST_ROOT = ../../priv/
JAVA_CLASS_SUBDIR = Orber/

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(ORBER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/orber-$(VSN)

#
# JAVA macros
#
JAVA_CLASSES = \
	InitialReference

TARGET_FILES= $(JAVA_CLASSES:%=$(JAVA_DEST_ROOT)$(JAVA_CLASS_SUBDIR)%.class)
JAVA_FILES= $(JAVA_CLASSES:%=%.java)

CLASSPATH = ../..

# ----------------------------------------------------
# Flags
# ----------------------------------------------------
JAVA_OPTIONS = 

# ----------------------------------------------------
# Make Rules
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) *~

docs:

# ----------------------------------------------------
# Release Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/java_src/Orber
	$(INSTALL_DATA) $(JAVA_FILES) $(RELSYSDIR)/java_src/Orber
	$(INSTALL_DIR) $(RELSYSDIR)/priv/Orber
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/priv/Orber

release_docs_spec:


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

#JAVA_DEST_ROOT =
#JAVA_CLASS_SUBDIR = 

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(JIVE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/jive-$(VSN)

#
# Common macros
#
JAVA_CLASSES = \
	Timer 

TARGET_FILES = $(JAVA_CLASSES:%=%.class)
JAVA_FILES = $(JAVA_CLASSES:%=%.java)
EXTRA_FILES = README timer.html

CLASSPATH = ../../java_src

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
	$(INSTALL_DIR) $(RELSYSDIR)/examples/applet
	$(INSTALL_DATA) $(TARGET_FILES) $(JAVA_FILES) $(EXTRA_FILES) \
		$(RELSYSDIR)/examples/applet

release_docs_spec:


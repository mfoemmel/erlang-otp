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

JAVA_DEST_ROOT = $(ERL_TOP)/lib/jinterface/priv/
JAVA_SRC_ROOT = $(ERL_TOP)/lib/jinterface/java_src/
JAVA_CLASS_SUBDIR = com/ericsson/otp/erlang/

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include $(ERL_TOP)/lib/jinterface/vsn.mk
VSN=$(JINTERFACE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/jinterface-$(VSN)

#
# JAVA macros
#
JAVA_CLASSES = \
	OtpException \
	OtpErlangException \
	OtpErlangDataException \
	OtpErlangDecodeException \
	OtpErlangRangeException \
	OtpErlangExit \
	OtpAuthException \
	OtpExternal \
	OtpInputStream \
	OtpOutputStream \
	OtpNode \
	OtpErlangList \
	OtpErlangTuple \
	OtpErlangBinary \
	OtpErlangAtom \
	OtpErlangBoolean \
	OtpErlangByte \
	OtpErlangChar \
	OtpErlangDouble \
	OtpErlangFloat \
	OtpErlangLong \
	OtpErlangInt \
	OtpErlangUInt \
	OtpErlangShort\
	OtpErlangUShort\
	OtpErlangString\
	OtpErlangObject \
	OtpErlangPid \
	OtpErlangPort \
	OtpErlangRef \
	OtpMD5 \
	OtpConnection \
	OtpPeer \
	OtpSelf \
	OtpServer \
	OtpEpmd

TARGET_FILES= $(JAVA_CLASSES:%=$(JAVA_DEST_ROOT)$(JAVA_CLASS_SUBDIR)%.class)
JAVA_FILES= $(JAVA_CLASSES:%=%.java)

JARFILE= OtpErlang.jar


# ----------------------------------------------------
# Programs and Flags
# ----------------------------------------------------

JAR= jar

CLASSPATH = $(JAVA_SRC_ROOT)

JAVADOCFLAGS=-d $(DOCDIR)
JAVAFLAGS=-d $(JAVA_DEST_ROOT) 
JARFLAGS=-cvf

JAVA_OPTIONS = 

# ----------------------------------------------------
# Make Rules
# ----------------------------------------------------

debug opt: $(JAVA_DEST_ROOT)$(JARFILE)

$(JAVA_DEST_ROOT)$(JARFILE): $(TARGET_FILES)
	@(cd $(JAVA_DEST_ROOT) ; $(JAR) $(JARFLAGS) $(JARFILE) $(JAVA_CLASS_SUBDIR))

clean:
	rm -f $(TARGET_FILES) *~

docs:

# ----------------------------------------------------
# Release Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/java_src/com/ericsson/otp/erlang
	$(INSTALL_DATA) $(JAVA_FILES) $(RELSYSDIR)/java_src/com/ericsson/otp/erlang
	$(INSTALL_DIR) $(RELSYSDIR)/priv
	$(INSTALL_DATA) $(JAVA_DEST_ROOT)$(JARFILE) $(RELSYSDIR)/priv

release_docs_spec:


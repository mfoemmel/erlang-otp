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

include ../vsn.mk
VSN = $(COMMON_TEST_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/common_test-$(VSN)

ifeq ($(findstring linux,$(TARGET)),linux)
XNIX = true
else
ifeq ($(findstring solaris,$(TARGET)),solaris)
XNIX = true
else
XNIX = false
endif
endif

ifneq ($(findstring win32,$(TARGET)),win32)

# ====================================================
# UNIX / LINUX
# ====================================================

#
# Files
#
FILES = vts.tool run_test.in
SCRIPTS = ../install.sh
SOLIBS = lib/erl_rx_driver.so

#
# Rules
#

include ../../test_server/vsn.mk
debug opt:
	sed -e 's;@CT_VSN@;$(VSN);' \
            -e 's;@TS_VSN@;$(TEST_SERVER_VSN);' \
            ../install.sh.in > ../install.sh
	chmod 775 ../install.sh

docs:

clean:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

ifeq ($(XNIX),true)
release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/bin
	$(INSTALL_DIR) $(RELSYSDIR)/priv/lib
	$(INSTALL_SCRIPT) $(SCRIPTS) $(RELSYSDIR)
	$(INSTALL_DATA) $(FILES) $(RELSYSDIR)/priv
	$(INSTALL_PROGRAM) $(SOLIBS) $(RELSYSDIR)/priv/lib
else
release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/bin
	$(INSTALL_DIR) $(RELSYSDIR)/priv/lib
	$(INSTALL_SCRIPT) $(SCRIPTS) $(RELSYSDIR)
	$(INSTALL_DATA) $(FILES) $(RELSYSDIR)/priv
endif

release_docs_spec:

else

# ====================================================
# WIN32
# ====================================================

#
# Files
#
FILES = vts.tool 

#
# Rules
#

debug opt:

docs:

clean:


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/bin
	$(INSTALL_DIR) $(RELSYSDIR)/priv/lib
	$(INSTALL_DATA) $(FILES) $(RELSYSDIR)/priv

release_docs_spec:

endif
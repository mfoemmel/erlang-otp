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
include ../vsn.mk
VSN=$(WEBTOOL_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/webtool-$(VSN)

# ----------------------------------------------------
# Target Specs
# ---------------------------------------------------- 

CONFIG_FILE = config.file

WEBSERVER_CONFIG_FILES = root/conf/httpd.conf \
                         root/conf/mime.types

HTDOCS_FILES =  root/doc/index.html \
	        root/doc/tool_management.html \
	        root/doc/start_info.html 

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: 

clean:

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/priv
	$(INSTALL_DIR)  $(RELSYSDIR)/priv/root
	$(INSTALL_DIR)  $(RELSYSDIR)/priv/root/conf
	$(INSTALL_DIR)  $(RELSYSDIR)/priv/root/doc
	$(INSTALL_DATA) $(CONFIG_FILE) $(RELSYSDIR)/priv
	$(INSTALL_DATA) $(HTDOCS_FILES) $(RELSYSDIR)/priv/root/doc
	$(INSTALL_DATA) $(WEBSERVER_CONFIG_FILES) $(RELSYSDIR)/priv/root/conf

release_docs_spec:



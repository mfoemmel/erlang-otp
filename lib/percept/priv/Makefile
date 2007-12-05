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
VSN=$(PERCEPT_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/percept-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
CONF_FILES = \
	server_root/conf/mime.types 

HTDOCS_FILES = \
	server_root/htdocs/index.html \
	server_root/htdocs/guide.html

IMAGE_FILES = 

SCRIPT_FILES = \
	server_root/scripts/percept_area_select.js \
	server_root/scripts/percept_error_handler.js \
	server_root/scripts/percept_select_all.js

CSS_FILES = \
	server_root/css/percept.css	


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
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
	# Finished
	$(INSTALL_DIR) $(RELSYSDIR)/priv/logs
	$(INSTALL_DIR) $(RELSYSDIR)/priv/server_root
	$(INSTALL_DIR) $(RELSYSDIR)/priv/server_root/htdocs
	$(INSTALL_DATA) $(HTDOCS_FILES) $(RELSYSDIR)/priv/server_root/htdocs
	$(INSTALL_DIR) $(RELSYSDIR)/priv/server_root/conf
	$(INSTALL_DATA) $(CONF_FILES) $(RELSYSDIR)/priv/server_root/conf
	$(INSTALL_DIR) $(RELSYSDIR)/priv/server_root/scripts
	$(INSTALL_DATA) $(SCRIPT_FILES) $(RELSYSDIR)/priv/server_root/scripts
	$(INSTALL_DIR) $(RELSYSDIR)/priv/server_root/css
	$(INSTALL_DATA) $(CSS_FILES) $(RELSYSDIR)/priv/server_root/css
	$(INSTALL_DIR) $(RELSYSDIR)/priv/server_root/images
#	$(INSTALL_DATA) $(IMAGE_FILES) $(RELSYSDIR)/priv/server_root/images
	
release_docs_spec:


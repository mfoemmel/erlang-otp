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
VSN=$(INETS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/inets-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES= ftp \
	httpd \
	httpd_conf \
	httpd_example \
	httpd_manager \
	httpd_listener \
	httpd_parse \
	httpd_request \
	httpd_response \
	httpd_socket \
	httpd_util \
	httpd_verbosity \
	inets_sup \
	mod_actions \
	mod_alias \
	mod_auth \
	mod_auth_plain \
	mod_auth_dets \
	mod_auth_mnesia \
	mod_security \
	mod_cgi \
	mod_dir \
	mod_disk_log \
	mod_esi \
	mod_get \
	mod_head \
	mod_include \
	mod_log

HRL_FILES= httpd.hrl mod_auth.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= inets.app
APPUP_FILE= inets.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
# DONT_USE_VERBOSITY = -Ddont_use_verbosity=true
INETS_FLAGS = -D'SERVER_SOFTWARE="inets/$(VSN)"' \
              -Ddefault_verbosity=info \
              $(DONT_USE_VERBOSITY)

# INETS_DEBUG_DEFAULT = l
ifeq ($(INETS_DEBUG),)
  INETS_DEBUG = $(INETS_DEBUG_DEFAULT)
endif

ifeq ($(INETS_DEBUG),c)
  INETS_FLAGS += -Dinets_cdebug -Dinets_debug -Dinets_log -Dinets_error
endif
ifeq ($(INETS_DEBUG),d)
  INETS_FLAGS += -Dinets_debug -Dinets_log -Dinets_error
endif
ifeq ($(INETS_DEBUG),l)
  INETS_FLAGS += -Dinets_log -Dinets_error
endif
ifeq ($(INETS_DEBUG),e)
  INETS_FLAGS += -Dinets_error
endif

ERL_FLAGS += 
ERL_COMPILE_FLAGS += $(INETS_FLAGS)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(HRL_FILES) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

info:
	@echo "INETS_DEBUG       = $(INETS_DEBUG)"
	@echo "INETS_FLAGS       = $(INETS_FLAGS)"
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"

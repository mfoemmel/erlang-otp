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
VSN=$(SSL_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ssl-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

MODULES= \
	ssl \
	ssl_alert \
	ssl_app \
	ssl_broker \
	ssl_broker_sup \
	ssl_server \
	ssl_sup \
	ssl_prim \
	ssl_pkix \
	ssl_pem	\
	ssl_base64 \
	inet_ssl_dist \
	ssl_certificate\
	ssl_certificate_db\
	ssl_cipher \
	ssl_connection \
	ssl_connection_sup \
	ssl_debug \
	ssl_handshake \
	ssl_manager \
	ssl_session \
	ssl_record \
	ssl_ssl2 \
	ssl_ssl3 \
	ssl_tls1 \

INTERNAL_HRL_FILES = \
	 ssl_int.hrl ssl_broker_int.hrl \
	 ssl_alert.hrl ssl_cipher.hrl ssl_handshake.hrl ssl_internal.hrl \
	 ssl_record.hrl

PUBLIC_HRL_FILES = ssl_pkix.hrl 

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE= ssl.app
APPUP_FILE= ssl.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

INCLUDE = ../include

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
EXTRA_ERLC_FLAGS = +warn_unused_vars
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/kernel/src -I$(INCLUDE) \
	$(EXTRA_ERLC_FLAGS) -DVSN=\"$(VSN)\"


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(PUBLIC_HRL_FILES)

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) 
	rm -f errs core *~

$(APP_TARGET):	$(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET):	$(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(PUBLIC_HRL_FILES):
	cp -f $(PUBLIC_HRL_FILES) $(INCLUDE)

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(PUBLIC_HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) \
	$(APPUP_TARGET) $(RELSYSDIR)/ebin

release_docs_spec:








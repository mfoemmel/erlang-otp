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

EBIN = ../../ebin
MEGACO_INCLUDEDIR = ../../include

include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MEGACO_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/megaco-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

ERL_FILES = $(MODULES:%=%.erl)	

TARGET_FILES =  \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE   = megaco.app
APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

APPUP_FILE   = megaco.appup
APPUP_SRC    = $(APPUP_FILE).src
APPUP_TARGET = $(EBIN)/$(APPUP_FILE)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

# ifeq ($(USE_HIPE),true)
# ERL_COMPILE_FLAGS += +native
# endif

ERL_FLAGS += -pa $(ERL_TOP)/lib/et/ebin \
             -pa $(ERL_TOP)/lib/megaco/ebin

ERL_COMPILE_FLAGS += -I../../include \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}'


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET)
	rm -f errs core *~

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@



# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(APP_TARGET) $(APPUP_TARGET) $(RELSYSDIR)/ebin
	$(INSTALL_DIR)  $(RELSYSDIR)/src
	$(INSTALL_DIR)  $(RELSYSDIR)/src/engine
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) $(RELSYSDIR)/src/engine
	$(INSTALL_DIR)  $(RELSYSDIR)/include
	$(INSTALL_DATA) $(EXTERNAL_HRL_FILES) $(RELSYSDIR)/include


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

$(EBIN)/megaco.$(EMULATOR): megaco.erl \
	../../include/megaco.hrl megaco_internal.hrl megaco_message_internal.hrl 

$(EBIN)/megaco_config.$(EMULATOR): megaco_config.erl \
	../../include/megaco.hrl megaco_internal.hrl

$(EBIN)/megaco_digit_map.$(EMULATOR): megaco_digit_map.erl \
	megaco_message_internal.hrl ../text/megaco_text_tokens.hrl

$(EBIN)/megaco_erl_dist_encoder.$(EMULATOR): megaco_erl_dist_encoder.erl \
	megaco_message_internal.hrl 

$(EBIN)/megaco_filter.$(EMULATOR): megaco_filter.erl \
	../../include/megaco.hrl ../../include/megaco_message_v1.hrl \
	megaco_internal.hrl 
#$(ET_TOP)/include/et.hrl

$(EBIN)/megaco_monitor.$(EMULATOR): megaco_monitor.erl 

$(EBIN)/megaco_misc_sup.$(EMULATOR): megaco_misc_sup.erl 

$(EBIN)/megaco_sup.$(EMULATOR): megaco_sup.erl

$(EBIN)/megaco_messenger.$(EMULATOR): megaco_messenger.erl \
	megaco_internal.hrl megaco_message_internal.hrl 

$(EBIN)/megaco_user.$(EMULATOR): megaco_user.erl \
	../../include/megaco.hrl ../../include/megaco_message_v1.hrl


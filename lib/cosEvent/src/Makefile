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
EBIN=../ebin
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(COSEVENT_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/cosEvent-$(VSN)
# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES = \
	OrberEventChannelAdmin_EventChannel_impl \
	CosEventChannelAdmin_SupplierAdmin_impl \
	CosEventChannelAdmin_ConsumerAdmin_impl \
	OrberEventChannelAdmin_ProxyPullConsumer_impl \
	OrberEventChannelAdmin_ProxyPullSupplier_impl \
	OrberEventChannelAdmin_ProxyPushConsumer_impl \
	OrberEventChannelAdmin_ProxyPushSupplier_impl \
	OrberEventChannel_EventChannelFactory_impl

ERL_FILES = $(MODULES:%=%.erl) 
HRL_FILES = event_service.hrl


GEN_ERL_FILES1 = \
	oe_CosEventChannelAdmin.erl \
	CosEventChannelAdmin_ConsumerAdmin.erl \
	CosEventChannelAdmin_EventChannel.erl \
	CosEventChannelAdmin_ProxyPullConsumer.erl \
	CosEventChannelAdmin_ProxyPullSupplier.erl \
	CosEventChannelAdmin_ProxyPushConsumer.erl \
	CosEventChannelAdmin_ProxyPushSupplier.erl \
	CosEventChannelAdmin_SupplierAdmin.erl

GEN_ERL_FILES2 = \
	oe_OrberEventChannelAdmin.erl \
	OrberEventChannelAdmin_EventChannel.erl \
	OrberEventChannelAdmin_ProxyPullConsumer.erl \
	OrberEventChannelAdmin_ProxyPullSupplier.erl \
	OrberEventChannelAdmin_ProxyPushConsumer.erl \
	OrberEventChannelAdmin_ProxyPushSupplier.erl \
	CosEventChannelAdmin_AlreadyConnected.erl \
	CosEventChannelAdmin_TypeError.erl

GEN_ERL_FILES3 = \
	oe_event_channel_factory.erl \
	OrberEventChannel_EventChannelFactory.erl

GEN_ERL_FILES4 = \
	oe_CosEventComm.erl \
	CosEventComm_Disconnected.erl \
	CosEventComm_PullConsumer.erl \
	CosEventComm_PullSupplier.erl \
	CosEventComm_PushConsumer.erl \
	CosEventComm_PushSupplier.erl

GEN_ERL_FILES =  \
	$(GEN_ERL_FILES1) $(GEN_ERL_FILES2) $(GEN_ERL_FILES3) $(GEN_ERL_FILES4)

GEN_HRL_FILES1 = \
	oe_CosEventChannelAdmin.hrl \
	CosEventChannelAdmin.hrl \
	CosEventChannelAdmin_ConsumerAdmin.hrl \
	CosEventChannelAdmin_EventChannel.hrl \
	CosEventChannelAdmin_ProxyPullConsumer.hrl \
	CosEventChannelAdmin_ProxyPullSupplier.hrl \
	CosEventChannelAdmin_ProxyPushConsumer.hrl \
	CosEventChannelAdmin_ProxyPushSupplier.hrl \
	CosEventChannelAdmin_SupplierAdmin.hrl

GEN_HRL_FILES2 = \
	oe_OrberEventChannelAdmin.hrl \
	OrberEventChannel.hrl \
	OrberEventChannelAdmin.hrl \
	OrberEventChannelAdmin_EventChannel.hrl \
	OrberEventChannelAdmin_ProxyPullConsumer.hrl \
	OrberEventChannelAdmin_ProxyPullSupplier.hrl \
	OrberEventChannelAdmin_ProxyPushConsumer.hrl \
	OrberEventChannelAdmin_ProxyPushSupplier.hrl \

GEN_HRL_FILES3 = \
	oe_event_channel_factory.hrl \
	OrberEventChannel_EventChannelFactory.hrl

GEN_HRL_FILES4 = \
	oe_CosEventComm.hrl \
	CosEventComm.hrl \
	CosEventComm_PullConsumer.hrl \
	CosEventComm_PullSupplier.hrl \
	CosEventComm_PushConsumer.hrl \
	CosEventComm_PushSupplier.hrl

GEN_HRL_FILES = \
	$(GEN_HRL_FILES1) $(GEN_HRL_FILES2) $(GEN_HRL_FILES3) $(GEN_HRL_FILES4)

TARGET_FILES = \
	$(GEN_ERL_FILES:%.erl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR)) 

GEN_FILES = $(GEN_HRL_FILES) $(GEN_ERL_FILES)

IDL_FILES = \
	CosEventChannelAdmin.idl \
	CosEventComm.idl \
	OrberEventChannelAdmin.idl \
	event_channel_factory.idl

APPUP_FILE = cosEvent.appup

APP_FILE = cosEvent.app
APP_SRC = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ERL_TOP)/lib/cosEvent/ebin -pa $(ERL_TOP)/lib/ic/ebin
# The -pa option is just used temporary until erlc can handle 
# includes from other directories than ../include .
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/cosEvent/include \
	+'{parse_transform, sys_pre_attributes}' \
	+'{attribute, insert, app_vsn, "cosEvent_$(COSEVENT_VSN)"}' 

YRL_FLAGS = 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug opt: $(TARGET_FILES) $(APP_TARGET)

clean:
	rm -f $(TARGET_FILES) $(GEN_FILES) $(APP_TARGET)
	rm -f errs core *~

$(APP_TARGET): $(APP_SRC)
	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(GEN_ERL_FILES1) $(GEN_HRL_FILES1): CosEventChannelAdmin.idl
	erlc $(ERL_FLAGS) +'{handle_info, "CosEventChannelAdmin::ConsumerAdmin"}' \
		+'{handle_info, "CosEventChannelAdmin::SupplierAdmin"}' \
		CosEventChannelAdmin.idl

$(GEN_ERL_FILES2) $(GEN_HRL_FILES2): OrberEventChannelAdmin.idl
	erlc $(ERL_FLAGS) +'handle_info' \
		+'{this, "OrberEventChannelAdmin::EventChannel::for_consumers"}' \
		+'{this, "OrberEventChannelAdmin::EventChannel::for_suppliers"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPullConsumer::connect_pull_supplier"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPullConsumer::disconnect_pull_consumer"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPullSupplier::connect_pull_consumer"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPullSupplier::disconnect_pull_supplier"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPushConsumer::connect_push_supplier"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPushConsumer::disconnect_push_consumer"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPushSupplier::connect_push_consumer"}' \
		+'{this, "OrberEventChannelAdmin::ProxyPushSupplier::disconnect_push_supplier"}' \
		OrberEventChannelAdmin.idl


$(GEN_ERL_FILES3) $(GEN_HRL_FILES3): event_channel_factory.idl
	erlc $(ERL_FLAGS) event_channel_factory.idl

$(GEN_ERL_FILES4) $(GEN_HRL_FILES4): CosEventComm.idl
	erlc $(ERL_FLAGS) CosEventComm.idl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(GEN_ERL_FILES) $(IDL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(GEN_HRL_FILES) $(RELSYSDIR)/include


release_docs_spec:







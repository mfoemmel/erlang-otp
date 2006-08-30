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

ifdef BOOTSTRAP
EGEN=$(BOOTSTRAP_TOP)/lib/kernel/egen
EBIN=$(BOOTSTRAP_TOP)/lib/kernel/ebin
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------

# Include erts/system/vsn.mk to port number for EPMD -- we will
# get an unwanted definition for VSN too.  Therefore,
# we'll use KERNEL_VSN directly instead of assigning it to
# VSN which is done in other Makefiles.  Same with HIPE_VSN.

include ../vsn.mk
include $(ERL_TOP)/erts/vsn.mk
include $(ERL_TOP)/lib/hipe/vsn.mk

include $(ERL_TOP)/erts/epmd/epmd.mk


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/kernel-$(KERNEL_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

PRE_LOADED_MODULES = \
	erl_prim_loader \
	init \
	prim_file \
	prim_inet \
	otp_ring0 \
	erlang

MODULES = \
	application \
	application_controller \
	application_master \
	application_starter \
	auth \
	code \
	code_aux \
	code_server \
	disk_log \
	disk_log_1 \
	disk_log_server \
	disk_log_sup \
	dist_ac \
	dist_util \
	erl_boot_server \
	erl_ddll \
	erl_distribution \
	erl_epmd \
	erl_reply \
	erts_debug \
	error_handler \
	error_logger \
	file \
	file_io_server \
	file_server \
	old_file_server \
	gen_tcp \
	gen_udp \
	global \
	global_group \
	global_search \
	group \
	heart \
	hipe_unified_loader \
	inet \
	inet6_tcp \
	inet6_tcp_dist \
	inet6_udp \
	inet_config \
	inet_db \
	inet_dns \
	inet_gethost_native \
	inet_hosts \
	inet_parse \
	inet_res \
	inet_tcp \
	inet_tcp_dist \
	inet_udp \
	kernel \
	kernel_config \
	net \
	net_adm \
	net_kernel \
	os \
	packages \
	pg2 \
	ram_file \
	rpc \
	seq_trace \
	user \
	user_drv \
	user_sup \
	wrap_log_reader \
	zlib

HRL_FILES= ../include/file.hrl ../include/inet.hrl
INTERNAL_HRL_FILES= application_master.hrl disk_log.hrl \
        net_address.hrl inet_dns.hrl inet_res.hrl \
        inet_boot.hrl inet_config.hrl inet_int.hrl \
	dist.hrl dist_util.hrl

ERL_FILES= $(PRE_LOADED_MODULES:%=%.erl) $(MODULES:%=%.erl)

NON_PRECIOUS_TARGETS = $(MODULES:%=$(EBIN)/%.$(EMULATOR)) \
		       $(APP_TARGET) $(APPUP_TARGET)

TARGET_FILES = $(PRE_LOADED_MODULES:%=$(EBIN)/%.$(EMULATOR)) $(NON_PRECIOUS_TARGETS)

APP_FILE= kernel.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= kernel.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ERL_FLAGS += 
ERL_COMPILE_FLAGS += -I../include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) 

# Note: In the open-source build clean must not destroyed the preloaded
# beam files.
clean:
	rm -f $(NON_PRECIOUS_TARGETS)
	rm -f core


docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

../../hipe/main/hipe.hrl: ../../hipe/vsn.mk ../../hipe/main/hipe.hrl.src
	sed -e "s;%VSN%;$(HIPE_VSN);" ../../hipe/main/hipe.hrl.src > ../../hipe/main/hipe.hrl

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(KERNEL_VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(KERNEL_VSN);' $< > $@


EPMD_FLAGS = -Depmd_port_no=$(EPMD_PORT_NO)	\
             -Depmd_node_type=$(EPMD_NODE_TYPE)	\
             -Depmd_dist_low=$(EPMD_DIST_LOW)	\
             -Depmd_dist_high=$(EPMD_DIST_HIGH)	\
             -Derlang_daemon_port=$(EPMD_PORT_NO)

$(EBIN)/erl_epmd.beam: $(ESRC)/erl_epmd.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) $(EPMD_FLAGS) \
		-o$(EBIN) $<

$(EBIN)/hipe_unified_loader.beam: ../../hipe/main/hipe.hrl hipe_ext_format.hrl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:


list_preloaded:
	@echo $(PRE_LOADED_MODULES)
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
VSN=$(STDLIB_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/stdlib-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES= \
	bplus_tree \
	c \
	calendar \
	dets \
	dict \
	digraph \
	digraph_utils \
	edlin \
	epp \
	erl_compile \
	erl_eval \
	erl_id_trans \
	erl_internal \
	erl_lint \
	erl_parse \
	erl_posix_msg \
	erl_pp \
	erl_scan \
	erl_tar \
	error_logger_file_h \
	error_logger_tty_h \
	ets \
	filelib \
	filename \
	gen \
	gen_event \
	gen_fsm \
	gen_server \
	io \
	io_lib \
	io_lib_format \
	io_lib_fread \
	io_lib_pretty \
	lib \
	lists \
	log_mf_h \
	math \
	otp_internal \
	ordsets \
	pg \
	pool \
	proc_lib \
	queue \
	random \
	regexp \
	shell \
	shell_default \
	slave \
	string \
	supervisor \
	supervisor_bridge \
	sys \
	timer \
	unix \
	win32reg

HRL_FILES= ../include/erl_compile.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= stdlib.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= stdlib.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -I../include -I../../kernel/include

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
	$(INSTALL_DATA) $(ERL_FILES) erl_parse.yrl $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/include
	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:







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
# For an outline of how this all_SUITE_data stuff works, see the
# make file ../../ssl/test/Makefile.
#
include $(ERL_TOP)/make/target.mk

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(INETS_VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

CP = cp

ifeq ($(TESTROOT_DIR),)
TESTROOT_DIR = /ldisk/tests/$(USER)/inets
endif

ifeq ($(INETS_DATA_DIR),)
INETS_DATA_DIR = $(TESTROOT_DIR)/data_dir
endif

ifeq ($(INETS_PRIV_DIR),)
INETS_PRIV_DIR = $(TESTROOT_DIR)/priv_dir
endif

INETS_FLAGS = -Dinets_data_dir='"$(INETS_DATA_DIR)"'       \
              -Dinets_priv_dir='"$(INETS_PRIV_DIR)"'


### 
### test suite debug flags
### 
ifeq ($(FTP_DEBUG_CLIENT),)
  FTP_DEBUG_CLIENT = y
endif

ifeq ($(FTP_DEBUG_CLIENT),y)
  FTP_FLAGS += -Dftp_debug_client
endif

ifneq ($(FTP_DEBUG),)
  FTP_DEBUG = s
endif

ifeq ($(FTP_DEBUG),l)
  FTP_FLAGS += -Dftp_log
endif

ifeq ($(FTP_DEBUG),d)
  FTP_FLAGS += -Dftp_debug -Dftp_log
endif

ifeq ($(INETS_DEBUG),)
  INETS_DEBUG = d
endif

ifeq ($(INETS_DEBUG),l)
  INETS_FLAGS += -Dinets_log
endif

ifeq ($(INETS_DEBUG),d)
  INETS_FLAGS += -Dinets_debug -Dinets_log
endif


### 
### HTTPD verbosity flags
### 

ifneq ($(MANV),)
  INETS_FLAGS += -Dhttpd_manager_verbosity=$(MANV)
else
  INETS_FLAGS += -Dhttpd_manager_verbosity=trace
endif

ifneq ($(REQV),)
  INETS_FLAGS += -Dhttpd_request_handler_verbosity=$(REQV)
else
  INETS_FLAGS += -Dhttpd_request_handler_verbosity=trace
endif

ifneq ($(ACCV),)
  INETS_FLAGS += -Dhttpd_acceptor_verbosity=$(ACCV)
else
  INETS_FLAGS += -Dhttpd_acceptor_verbosity=trace
endif

ifneq ($(AUTHV),)
  INETS_FLAGS += -Dhttpd_auth_verbosity=$(AUTHV)
else
  INETS_FLAGS += -Dhttpd_auth_verbosity=log
endif

ifneq ($(SECV),)
  INETS_FLAGS += -Dhttpd_security_verbosity=$(SECV)
else
  INETS_FLAGS += -Dhttpd_security_verbosity=log
endif

INETS_ROOT = ../../inets

MODULES =                 \
        inets_test_server \
	inets_test_lib    \
	httpd_test_lib    \
	inets_SUITE       \
        inets_app_test    \
        inets_appup_test  \
        httpd_test        \
        httpc_test        \
	ftp_test          \
        httpd_poll        \
        httpd_time_test

EBIN = .

HRL_FILES = inets_test_lib.hrl httpd_test_lib.hrl

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

INETS_FILES = Makefile.inets rules.mk suite_targets.mk \
              inets.config inets.spec inets.spec.vxworks 

SOURCE = $(ERL_FILES) $(HRL_FILES) $(TARGET_FILES)

INETS_SPECS = inets.spec inets.spec.vxworks

INETS_SUITE = inets_SUITE
APP_SUITE   = inets_app_test
APPUP_SUITE = inets_appup_test
FTP_SUITE   = ftp_test
HTTPD_SUITE = httpd_test
HTTPC_SUITE = httpc_test

HTTPD_DATADIR = httpd_test_data
DATADIRS = $(HTTPD_DATADIR)

EMAKEFILE  = Emakefile
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(INETS_SPECS) $(SOURCE) 
else
BUILDTARGET   = emakebuild
RELTEST_FILES = $(EMAKEFILE) $(INETS_SPECS) $(SOURCE) 
endif

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/inets-$(VSN)

RELTESTSYSDIR        = $(RELEASE_PATH)/inets_test
RELTESTSYSALLDATADIR = $(RELTESTSYSDIR)/all_SUITE_data
RELTESTSYSBINDIR     = $(RELTESTSYSALLDATADIR)/bin

MAKEFILE_SRC = Makefile.src
INETS_SSL_LIB_DIR  = ../../ssl/usr/ssleay/


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += $(FTP_FLAGS) $(INETS_FLAGS)


# ----------------------------------------------------
# Targets
# erl -sname kalle -pa ../ebin
# If you intend to run the test suite locally (private), then
# there is some requirements:
# 1) INETS_PRIV_DIR must be created
# 2) INETS_SSL_LIB_DIR must be created with the same content as 
#    content comparable to all_SUITE_data/bin (see test-server).
# ----------------------------------------------------
tests debug opt: $(BUILDTARGET)

targets: $(TARGET_FILES)

.PHONY: emakebuild

emakebuild: $(EMAKEFILE)
	erl $(ERL_FLAGS) -make

$(EMAKEFILE): 
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' > $(EMAKEFILE)
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) >> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core *~

docs:


# ----------------------------------------------------
# inets
# ---------------------------------------------------- 

include suite_targets.mk


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/test
	$(INSTALL_DATA) $(HRL_FILES)   $(ERL_FILES) $(RELSYSDIR)/test
	$(INSTALL_DATA) $(INETS_FILES) $(RELSYSDIR)/test
	$(INSTALL_DIR)  $(RELSYSDIR)/test/$(HTTPD_DATADIR)
	tar chf - -C $(HTTPD_DATADIR) . | (cd $(RELSYSDIR)/test/$(HTTPD_DATADIR); tar xf -)
	(cd $(RELSYSDIR)/test; mv Makefile.inets Makefile)

release_tests_spec: opt
	$(INSTALL_DIR) $(RELTESTSYSDIR)
	$(INSTALL_DATA) $(RELTEST_FILES) $(RELTESTSYSDIR)
	chmod -f -R u+w $(RELTESTSYSDIR)
	tar chf - $(DATADIRS) | (cd $(RELTESTSYSDIR); tar xf -)
	$(INSTALL_DIR) $(RELTESTSYSALLDATADIR)
	$(INSTALL_DATA) $(MAKEFILE_SRC) $(RELTESTSYSALLDATADIR)/Makefile.src
	$(INSTALL_DATA) copy_files.erl $(RELTESTSYSALLDATADIR)/copy_files.erl
	$(INSTALL_DIR) $(RELTESTSYSBINDIR)
	chmod -f -R +x $(RELTESTSYSBINDIR)
	tar cf - -C $(INETS_SSL_LIB_DIR) . | (cd $(RELTESTSYSALLDATADIR); tar xf -)
	$(INSTALL_DIR) $(RELTESTSYSALLDATADIR)/win32/lib	
	tar cf - -C $(INETS_SSL_LIB_DIR)/win32/out32dll . | \
		(cd $(RELTESTSYSALLDATADIR)/win32/lib; tar xf -)

release_docs_spec:

info:
	@echo "MAKE_EMAKE        = $(MAKE_EMAKE)"
	@echo "EMAKEFILE         = $(EMAKEFILE)"
	@echo "BUILDTARGET       = $(BUILDTARGET)"
	@echo "INETS_DATA_DIR    = $(INETS_DATA_DIR)"
	@echo "INETS_PRIV_DIR    = $(INETS_PRIV_DIR)"
	@echo "INETS_SSL_LIB_DIR = $(INETS_SSL_LIB_DIR)"
	@echo "INETS_ROOT        = $(INETS_ROOT)"
	@echo "INETS_FLAGS       = $(INETS_FLAGS)"
	@echo "FTP_FLAGS         = $(FTP_FLAGS)"

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
# --------------------------------------------------------------------
# If downloading this app separately (not as part of 
# an OTP source release), then there is two ways to
# build and install this app.
# 1) Replace the megaco source dir in an existing
#    OTP source tree. Then re-configure, build and 
#    install OTP.
#    Note that the directory name has to be renamed
#    from megaco-<version> to megaco.
# 2) Unpack outside an OTP-source tree. 
#    a) Configure app (make ERL_TOP=</path/to/top/OTP-source/dir> conf)
#    b) make ERL_TOP=</path/to/top/OTP-source/dir>
#    c) make ERL_TOP=</path/to/top/OTP-source/dir> \
#            OTP_INSTALL_DIR=</path/to/top/OTP-install/dir> app_install
#       (e.g. if otp is installed in /usr/local/otp-r9b,
#        then OTP_INSTALL_DIR is /usr/local/otp-r9b and
#        the app will be installed in dir 
#        /usr/local/otp-r9b/lib/erlang/lib/megaco-<version>)
# -----------------------------------------------------------------------
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include vsn.mk
VSN=$(MEGACO_VSN)

DIR_NAME = megaco-$(VSN)

ifndef APP_RELEASE_DIR
  APP_RELEASE_DIR = /tmp
endif

ifndef APP_TAR_FILE
  APP_TAR_FILE = $(APP_RELEASE_DIR)/$(DIR_NAME).tgz
endif

APP_DIR = $(APP_RELEASE_DIR)/$(DIR_NAME)

ifdef OTP_INSTALL_DIR
  APP_INSTALL_DIR = $(OTP_INSTALL_DIR)/lib/erlang
else
  # If installing into an OTP structure created
  # by installing an source OTP build, the '/tmp'
  # shall be replaced with the value of ERL_TOP
  APP_INSTALL_DIR = /tmp/lib/erlang
endif

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------

include subdirs.mk

SUB_DIRECTORIES = $(SUB_DIRS) doc/src

SPECIAL_TARGETS = 

ifeq ($(FLEX_SCANNER),disable)
  FLEX_SCANNER_LINENO = --disable-megaco-flex-scanner-lineno
else
  FLEX_SCANNER_LINENO = --enable-megaco-flex-scanner-lineno
endif

# ----------------------------------------------------
# Default Subdir Targets
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_subdir.mk

conf: do_configure

do_configure: configure 
	./configure $(FLEX_SCANNER_LINENO)

configure: configure.in
	autoconf


info:
	@echo "APP_RELEASE_DIR: $(APP_RELEASE_DIR)"
	@echo "APP_DIR:         $(APP_DIR)"
	@echo "APP_TAR_FILE:    $(APP_TAR_FILE)"
	@echo "OTP_INSTALL_DIR: $(OTP_INSTALL_DIR)"
	@echo "APP_INSTALL_DIR: $(APP_INSTALL_DIR)"

version:
	@echo "$(VSN)"


# ----------------------------------------------------
# Application install (of a app built from source) targets
# ----------------------------------------------------
app_install:
	$(MAKE) TESTROOT=$(APP_INSTALL_DIR) release 


# ----------------------------------------------------
# Application (source) release targets
# ----------------------------------------------------
app_release: app_doc tar

app_doc:
	cd doc/src; $(MAKE) html man

app_dir: $(APP_DIR)

$(APP_DIR):
	cat TAR.exclude > TAR.exclude2; \
        echo "megaco/TAR.exclude2" >> TAR.exclude2; \
        echo "megaco/priv/lib/$(TARGET)" >> TAR.exclude2; \
        echo "megaco/src/flex/$(TARGET)" >> TAR.exclude2
	(cd ..; find megaco -name 'findmerge.*' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*.contrib*' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*.keep*' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*~' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name 'erl_crash.dump' >> megaco/TAR.exclude2)
	(cd ..; find megaco/test -name '*.beam' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '*.log' >> megaco/TAR.exclude2)
	(cd ..; find megaco/examples/meas -name '*.xls' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name 'core' >> megaco/TAR.exclude2)
	(cd ..; find megaco -name '.cmake.state' >> megaco/TAR.exclude2)
	mkdir $(APP_DIR); \
        (cd ..; tar cfX - megaco/TAR.exclude2 megaco) | \
        (cd $(APP_DIR); tar xf -); \
        mv $(APP_DIR)/megaco/* $(APP_DIR)/; \
	mkdir $(APP_DIR)/autoconf; \
        cp autoconf/config.guess $(APP_DIR)/autoconf/; \
        cp autoconf/config.sub $(APP_DIR)/autoconf/; \
        cp autoconf/install-sh $(APP_DIR)/autoconf/; \
        rmdir $(APP_DIR)/megaco

tar: $(APP_TAR_FILE)

$(APP_TAR_FILE): $(APP_DIR)
	(cd $(APP_RELEASE_DIR); gtar zcf $(APP_TAR_FILE) $(DIR_NAME))


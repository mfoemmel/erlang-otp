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

ERL_FILES = \
	$(MODULES:%=%.erl) \
	$(INTERNAL_YRL_FILES:%.yrl=%.erl)

ERL_TARGET_FILES = \
	$(INTERNAL_YRL_FILES:%.yrl=%.erl)

BEAM_TARGET_FILES = \
	$(INTERNAL_YRL_FILES:%.yrl=$(EBIN)/%.$(EMULATOR)) \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))

TARGET_FILES = $(ERL_TARGET_FILES) $(BEAM_TARGET_FILES)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

ifeq ($(USE_MEGACO_HIPE),true)
ERL_COMPILE_FLAGS += +native
endif

ifeq ($(WARN_UNUSED_WARS),true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

ERL_FLAGS += -pa $(ERL_TOP)/lib/megaco/ebin
ERL_COMPILE_FLAGS += -I../../include \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}'


ifeq ($(YRL_VERBOSE),true)
YRL_FLAGS += -v
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info:
	@echo "MODULES            = $(MODULES)"
	@echo ""
	@echo "ERL_FILES          = $(ERL_FILES)"
	@echo ""
	@echo "ERL_TARGET_FILES   = $(ERL_TARGET_FILES)"
	@echo ""
	@echo "BEAM_TARGET_FILES  = $(BEAM_TARGET_FILES)"
	@echo ""
	@echo "TARGET_FILES       = $(TARGET_FILES)"
	@echo ""
	@echo "INTERNAL_YRL_FILES = $(INTERNAL_YRL_FILES)"
	@echo ""
	@echo "INTERNAL_HRL_FILES = $(INTERNAL_HRL_FILES)"
	@echo ""


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

parser: megaco_text_parser.$(EMULATOR)


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(BEAM_TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR)  $(RELSYSDIR)/src
	$(INSTALL_DIR)  $(RELSYSDIR)/src/text
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_YRL_FILES) $(INTERNAL_HRL_FILES) $(RELSYSDIR)/src/text


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

include depend.mk


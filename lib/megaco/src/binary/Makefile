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

ASN1_SPECS = $(ASN1_V1_SPEC) $(ASN1_V2_SPEC) 
ASN1_FILES = $(ASN1_SPECS:%=%.asn)

V1_SPECS = $(BER_ASN1_V1_SPEC)         \
           $(BER_BIN_ASN1_V1_SPEC)     \
           $(BER_BIN_DRV_ASN1_V1_SPEC) \
           $(PER_ASN1_V1_SPEC)         \
           $(PER_BIN_ASN1_V1_SPEC)     \
           $(PER_BIN_DRV_ASN1_V1_SPEC) 

V2_SPECS = $(BER_ASN1_V2_SPEC)         \
           $(BER_BIN_ASN1_V2_SPEC)     \
           $(BER_BIN_DRV_ASN1_V2_SPEC) \
           $(PER_ASN1_V2_SPEC)         \
           $(PER_BIN_ASN1_V2_SPEC)     \
           $(PER_BIN_DRV_ASN1_V2_SPEC) 

SPECS = $(V1_SPECS) $(V2_SPECS) 

V1_SPEC_BINS = $(V1_SPECS:%=%.erl) $(V1_SPECS:%=%.hrl) $(V1_SPECS:%=%.asn1db)
V2_SPEC_BINS = $(V2_SPECS:%=%.erl) $(V2_SPECS:%=%.hrl) $(V2_SPECS:%=%.asn1db)
SPEC_BINS    = $(V1_SPEC_BINS) $(V2_SPEC_BINS) 

ERL_FILES = $(MODULES:%=%.erl)	
HRL_FILES = $(INTERNAL_HRL_FILES) $(V1_SPECS:%=%.hrl) $(V2_SPECS:%=%.hrl)

TARGET_FILES =  \
	$(MODULES:%=$(EBIN)/%.$(EMULATOR))


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

ifeq ($(WARN_UNUSED_WARS),true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

ifeq ($(USE_MEGACO_HIPE),true)
ERL_COMPILE_FLAGS += +native
endif


ERL_FLAGS += -pa $(ERL_TOP)/lib/megaco/ebin
ERL_COMPILE_FLAGS += -I../../include \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}'


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
debug:
	@${MAKE} TYPE=debug opt

opt: $(TARGET_FILES) 

v1: $(V2_SPEC_BINS)

v2: $(V2_SPEC_BINS)

specs: v1 v2

clean:
	rm -f $(TARGET_FILES) 
	rm -f $(SPEC_BINS)
	rm -f errs core *~

docs:

info:
	@echo "MODULES:       $(MODULES)"
	@echo "ERL_FILES:     $(ERL_FILES)"
	@echo "HRL_FILES:     $(HRL_FILES)"
	@echo "TARGET_FILES:  $(TARGET_FILES)"
	@echo "ASN1_SPECS:    $(ASN1_SPECS)"
	@echo "SPECS:         $(SPECS)"
	@echo "SPEC_BINS:     $(SPEC_BINS)"


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR)  $(RELSYSDIR)/src
	$(INSTALL_DIR)  $(RELSYSDIR)/src/binary
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(ASN1_FILES) $(RELSYSDIR)/src/binary


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

include depend.mk






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

SPECS = $(BER_ASN1_SPEC)     \
        $(BER_BIN_ASN1_SPEC) \
        $(PER_ASN1_SPEC)     \
        $(PER_BIN_ASN1_SPEC) 

SPEC_BINS = $(SPECS:%=%.erl) $(SPECS:%=%.hrl) $(SPECS:%=%.asn1db)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug
endif

ifeq ($(WARN_UNUSED_WARS),true)
ERL_COMPILE_FLAGS += +warn_unused_vars
endif

# This is valid together with the -bber_bin and -bper_bin options
ifneq ($(DONT_USE_BIN_OPTIMIZE),true)
OPTIMIZE = +optimize
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

clean:
	rm -f $(TARGET_FILES) 
	rm -f $(SPEC_BINS)
	rm -f errs core *~

docs:

info:
	@echo "USE_PER_BIN:   $(USE_PER_BIN)"
	@echo "MODULES:       $(MODULES)"
	@echo "ERL_FILES:     $(ERL_FILES)"
	@echo "TARGET_FILES:  $(TARGET_FILES)"
	@echo "SPECS:         $(SPECS)"
	@echo "SPEC_BINS:     $(SPEC_BINS)"


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

ifeq ($(USE_PER_BIN_OPT),yes)
asn1: ber ber_bin per per_bin per_bin_opt
else
asn1: ber ber_bin per per_bin
endif

ber: 
	$(ERLC) -bber     $(BER_ASN1_SPEC).set.asn

ber_bin: 
	$(ERLC) -bber_bin $(BER_BIN_ASN1_SPEC).set.asn

per: 
	$(ERLC) -bper     $(PER_ASN1_SPEC).set.asn

per_bin: 
	$(ERLC) -bper_bin $(PER_BIN_ASN1_SPEC).set.asn

per_bin_opt: 
	$(ERLC) -bper_bin +optimize $(PER_BIN_OPT_ASN1_SPEC).set.asn


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk


release_spec: opt
	$(INSTALL_DIR)  $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR)  $(RELSYSDIR)/src
	$(INSTALL_DIR)  $(RELSYSDIR)/src/binary
	$(INSTALL_DATA) $(ERL_FILES) $(INTERNAL_HRL_FILES) $(ASN1_SPEC).asn $(RELSYSDIR)/src/binary


release_docs_spec:


# ----------------------------------------------------
# Include dependencies
# ----------------------------------------------------

include depend.mk






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

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif

EBIN = ../../ebin
MEGACO_INCLUDEDIR = ../../include

include $(ERL_TOP)/make/$(TARGET)/otp.mk


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(MEGACO_VSN)

# Must find some smarter way to figure out this
# per_bin included as of 1.3.2 of ASN.1
# USE_PER_BIN = $(shell (erlc megaco_asn1_make_util.erl; erl -noshell -run megaco_asn1_make_util asn1version_check 1.3.2 -s erlang halt; rm -f megaco_asn1_make_util.beam))

# Fix this later...
USE_PER_BIN = yes


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

SPECS = $(BER_ASN1_SPEC) \
        $(BER_BIN_ASN1_SPEC) \
        $(PER_ASN1_SPEC)
ifeq ($(USE_PER_BIN),yes)
SPECS += $(PER_BIN_ASN1_SPEC)
endif

SPEC_BINS = $(SPECS:%=%.erl) $(SPECS:%=%.hrl) $(SPECS:%=%.asn1db)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ERL_TOP)/lib/megaco/ebin
ERL_COMPILE_FLAGS += -I../../include 


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

asn1: ber ber_bin per # per_bin

ber: 
	$(ERLC) -bber     $(BER_ASN1_SPEC).set.asn

ber_bin: 
	$(ERLC) -bber_bin $(BER_BIN_ASN1_SPEC).set.asn

per: 
	$(ERLC) -bper     $(PER_ASN1_SPEC).set.asn

per_bin: 
	$(ERLC) -bper_bin $(PER_BIN_ASN1_SPEC).set.asn


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






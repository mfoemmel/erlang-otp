#
# Copyright (C) 2004, Ericsson Telecommunications
# Author: Richard Carlsson, Bertil Karlsson 
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(EDOC_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(VSN)


#
# Common Macros
#

EBIN = ../ebin

ifndef EDOC_INCLUDE
  EDOC_INCLUDE = ../../xmerl/include
endif

ERL_COMPILE_FLAGS += -I$(EDOC_INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	edoc.erl \
	edoc_scanner.erl \
	edoc_parser.erl \
	edoc_layout.erl \
	edoc_lib.erl \
	edoc_run.erl edoc_report.erl edoc_extract.erl edoc_tags.erl \
	edoc_macros.erl edoc_doclet.erl edoc_data.erl edoc_refs.erl \
	edoc_types.erl edoc_parse_expr.erl edoc_parse_ref.erl \
	edoc_parse_spec.erl edoc_parse_typedef.erl edoc_parse_xml.erl \
	edoc_parse_contact.erl edoc_wiki.erl \
	otpsgml_layout.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

HRL_FILES = edoc.hrl


debug opt: $(OBJECTS)

all: $(OBJECTS)

$(EBIN)/edoc.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_data.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_doclet.$(EMULATOR): $(EDOC_INCLUDE)/xmerl.hrl
$(EBIN)/edoc_extract.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_layout.$(EMULATOR): $(EDOC_INCLUDE)/xmerl.hrl
$(EBIN)/edoc_lib.$(EMULATOR): edoc.hrl $(EDOC_INCLUDE)/xmerl.hrl
$(EBIN)/edoc_macros.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_parse_xml.$(EMULATOR): $(EDOC_INCLUDE)/xmerl.hrl
$(EBIN)/edoc_parser.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_refs.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_report.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_tags.$(EMULATOR): edoc.hrl
$(EBIN)/edoc_types.$(EMULATOR): edoc.hrl $(EDOC_INCLUDE)/xmerl.hrl


clean:
	rm -f $(OBJECTS) edoc_parser.erl
	rm -f core *~

distclean: clean

realclean: clean

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<


# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(OBJECTS) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(SOURCES) $(HRL_FILES) $(RELSYSDIR)/src


release_docs_spec:


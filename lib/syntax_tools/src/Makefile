#
# Copyright (C) 2004, Ericsson Telecommunications
# Authors: Richard Carlsson, Bertil Karlsson
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(SYNTAX_TOOLS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/syntax_tools-$(VSN)


#
# Common Macros
#

EBIN = ../ebin

ERL_COMPILE_FLAGS += +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES=erl_syntax.erl erl_prettypr.erl erl_syntax_lib.erl	\
	erl_comment_scan.erl erl_recomment.erl erl_tidy.erl	\
	epp_dodger.erl prettypr.erl igor.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))


debug opt: $(OBJECTS)

all: $(OBJECTS)


clean:
	rm -f $(OBJECTS)
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
	$(INSTALL_DATA) $(SOURCES) $(RELSYSDIR)/src


release_docs_spec:


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
#     $Id$
#

ifndef EBIN
EBIN = ../ebin
endif

ifndef DOCS
DOCS = ../doc
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(HIPE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/hipe-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = hipe hipe_main


## hipe.hrl is automatically generated from hipe.hrl.src -- see below
HRL_FILES= hipe.hrl
ERL_FILES= $(MODULES:%=%.erl)
TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

APP_FILE= hipe.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= hipe.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +nowarn_shadow_vars +warn_obsolete_guard +warn_untyped_record +warn_missing_spec_exported

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

hipe.hrl: ../vsn.mk hipe.hrl.src
	sed -e "s;%VSN%;$(HIPE_VSN);" hipe.hrl.src > hipe.hrl

$(EBIN)/hipe.beam: hipe.hrl
$(EBIN)/hipe.beam: ../rtl/hipe_literals.hrl ../../compiler/src/beam_disasm.hrl
$(EBIN)/hipe_main.beam: hipe.hrl ../icode/hipe_icode.hrl #../rtl/hipe_rtl.hrl

debug opt: $(TARGET_FILES) 

docs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES) $(DOC_FILES) $(HRL_FILES)
	rm -f core

$(DOCS)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s init stop

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
	$(INSTALL_DATA) ../vsn.mk $(RELSYSDIR)
	$(INSTALL_DIR) $(RELSYSDIR)/main
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/main
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

#=============================================================================
#
#    File:  lib/dialyzer/src/Makefile
# Authors:  Kostis Sagonas and Tobias Lindahl
#
#=============================================================================

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(DIALYZER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/dialyzer-$(VSN)

# ----------------------------------------------------
# Orientation information -- find HiPE's icode dir
# ----------------------------------------------------
DIALYZER_DIR = $(ERL_TOP)/lib/dialyzer
HIPE_ICODE_DIR = $(ERL_TOP)/lib/hipe/icode

##---------------------------------------------------------------------
## Edit here if you want to change the default libraries included in
## Dialyzer's initial PLT (Persistent Lookup Table).
##
## 1. Minimal configuration -- compatible with Dialyzer version 1.3.0
##
DIALYZER_DEF_LIBS = "kernel,stdlib"
##
## 2. Configuration which includes enough stuff for most Dialyzer uses
## 
# DIALYZER_DEF_LIBS = "compiler,gs,hipe,kernel,mnesia,stdlib,syntax_tools"
##
## 3. Configuration without any unknown functions when building the PLT.
##    Gives good results for most uses, but takes really long to
##    create the initial PLT. If you have patience or a fast machine,
##    we strongly recommend this option. Also, if your application uses
##    more OTP libraries than those listed below, then by all means add
##    them to the list.
##
# DIALYZER_DEF_LIBS = "asn1,compiler,crypto,dialyzer,debugger,edoc,et,gs,hipe,inets,kernel,observer,mnesia,mnemosyne,parsetools,runtime_tools,snmp,stdlib,ssl,syntax_tools,tools,webtool,xmerl"
##---------------------------------------------------------------------

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES =  \
	dialyzer \
	dialyzer_analysis_callgraph \
	dialyzer_callgraph \
	dialyzer_cl \
	dialyzer_cl_parse \
	dialyzer_codeserver \
	dialyzer_dataflow \
	dialyzer_dep \
	dialyzer_gui \
	dialyzer_icode \
	dialyzer_options \
	dialyzer_plt \
	dialyzer_typesig

EXTRA_FILES= dialyzer.hrl ../plt/dialyzer_init_plt	## automatically generated

ERL_FILES= $(MODULES:%=%.erl)
INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
TARGET_FILES= $(INSTALL_FILES)
 
APP_FILE= dialyzer.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= dialyzer.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS +=
ERL_COMPILE_FLAGS += -I$(HIPE_ICODE_DIR) +warn_unused_import

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(DIALYZER_DIR)/plt/dialyzer_init_plt

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f $(EXTRA_FILES)
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/dialyzer_cl_parse.$(EMULATOR): dialyzer_cl_parse.erl
	erlc $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -Ddialyzer_def_libs="[$(DIALYZER_DEF_LIBS)]" -o$(EBIN) dialyzer_cl_parse.erl


$(DIALYZER_DIR)/plt/dialyzer_init_plt:
	@echo "Dialyzer will now build auxiliary information needed for subsequent analyses..."
	@echo "A PLT for following libs will be built:"
	@echo "  [$(DIALYZER_DEF_LIBS)]"
	@echo "To select a different set please edit the file $(DIALYZER_DIR)/src/Makefile"
	$(ERL_TOP)/bin/dialyzer --include_libs $(DIALYZER_DEF_LIBS) -I $(HIPE_ICODE_DIR) --command-line ../ebin

$(EBIN)/dialyzer_cl_parse.$(EMULATOR): dialyzer_cl_parse.erl
	erlc $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -Ddialyzer_def_libs="[$(DIALYZER_DEF_LIBS)]" -o$(EBIN) dialyzer_cl_parse.erl

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

dialyzer.hrl: dialyzer.hrl.src ../vsn.mk Makefile
	sed -e "s;%VSN%;$(VSN);" \
	    -e "s;%DEF_LIBS%;\[$(DIALYZER_DEF_LIBS)\];" \
	    -e "s;%DIALYZER_DIR%;\"$(DIALYZER_DIR)\";" \
		$< > $@

# ---------------------------------------------------------------------
# dependencies -- I wish they were somehow automatically generated
# ---------------------------------------------------------------------

$(EBIN)/dialyzer.beam: dialyzer.hrl
$(EBIN)/dialyzer_analysis.beam: dialyzer.hrl
$(EBIN)/dialyzer_analysis_callgraph.beam: dialyzer.hrl
$(EBIN)/dialyzer_cl.beam: dialyzer.hrl $(HIPE_ICODE_DIR)/hipe_icode_type.hrl
$(EBIN)/dialyzer_cl_parse.beam: dialyzer.hrl
$(EBIN)/dialyzer_dataflow.beam: dialyzer.hrl
$(EBIN)/dialyzer_gui.beam: dialyzer.hrl $(HIPE_ICODE_DIR)/hipe_icode_type.hrl
$(EBIN)/dialyzer_icode.beam: $(HIPE_ICODE_DIR)/hipe_icode.hrl $(HIPE_ICODE_DIR)/hipe_icode_type.hrl
$(EBIN)/dialyzer_options.beam: dialyzer.hrl $(HIPE_ICODE_DIR)/hipe_icode_type.hrl
$(EBIN)/dialyzer_plt.beam: dialyzer.hrl
$(EBIN)/dialyzer_typesig.beam: dialyzer.hrl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(EXTRA_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(INSTALL_FILES) $(RELSYSDIR)/ebin
	$(INSTALL_DIR) $(RELSYSDIR)/plt
	$(INSTALL_DATA) ../plt/dialyzer_init_plt $(RELSYSDIR)/plt

release_docs_spec:

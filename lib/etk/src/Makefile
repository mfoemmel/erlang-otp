
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk


#
# Makefile for tk erlang binaries
#
include ../vsn.mk
VSN=$(ETK_VSN)

ifeq ($(TYPE),debug)
ERL_COMPILE_FLAGS += -Ddebug -W
endif

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/etk-$(VSN)

MODULES = \
	tk \
	etk \
	etk_menu \
	tkbutton \
	tkconsole \
	tkdialog \
	tkentry \
	tkfocus \
	tklib \
	tklistbox \
	tkmenu \
	tkoptmenu \
	tkscale \
	tkscrlbar \
	tktearoff \
	tktext            



HRL_FILES=tk.hrl

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) 

APP_FILE= etk.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt: $(TARGET_FILES)

debug:
	@${MAKE} TYPE=debug

clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:



# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC)
	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------

$(TARGET_FILES):	$(HRL_FILES)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES)  $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

release_src_spec:
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(APP_SRC) $(ERL_FILES) $(HRL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/src
	$(INSTALL_DATA) ../README ../Makefile ../vsn.mk $(RELSYSDIR)

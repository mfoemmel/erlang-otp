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
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(GS_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/gs-$(VSN)

ERL = erl

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES=    	gs gs_frontend gs_make gs_widgets gtk gtk_arc gtk_button\
                gtk_canvas gtk_checkbutton gtk_db gtk_editor gtk_entry \
                gtk_font gtk_frame gtk_grid gtk_gridline gs_packer \
                gtk_gs gtk_image gtk_label gtk_line gtk_listbox gtk_menu\
                gtk_menubar gtk_menubutton gtk_menuitem gtk_oval gtk_polygon \
                gtk_port_handler gtk_radiobutton gtk_rectangle gtk_scale \
                gtk_text gtk_widgets gtk_window tcl2erl tool_utils tool_genfd \
		tool_file_dialog gse

GTK_GENERIC = gtk_generic.erl

HRL_FILES = gtk.hrl
GEN_HRL_FILES = gtk_generic.hrl
GTK_GENERIC_TARGET = $(EBIN)/gtk_generic.$(EMULATOR)
KEEP_AS_IS = $(EBIN)/gs-color-xdefaults $(EBIN)/fup.bm

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(GEN_HRL_FILES) \
	$(GTK_GENERIC_TARGET) $(KEEP_AS_IS) $(APP_TARGET)

APP_FILE= gs.app
APP_SRC= $(APP_FILE).src
APP_TARGET= ../ebin/$(APP_FILE)


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS +=

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core *~

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

gtk_generic.hrl: gs_make.erl
	$(ERL) -pa $(EBIN) -s gs_make -s erlang halt -noshell

$(EBIN)/gs-color-xdefaults: gs-color-xdefaults
	rm -f $@
	cp gs-color-xdefaults $@

$(EBIN)/fup.bm: fup.bm
	rm -f $@
	cp fup.bm $@

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(APP_SRC) $(ERL_FILES) $(HRL_FILES) $(GEN_HRL_FILES) \
		$(GTK_GENERIC) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(KEEP_AS_IS) $(RELSYSDIR)/ebin


release_docs_spec:


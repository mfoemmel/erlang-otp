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
include ../../vsn.mk
VSN=$(EI_VSN)

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

include make.dep

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/erl_interface-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

XML_REF1_FILES = erl_call.xml 
XML_REF3_FILES = erl_connect.xml \
	erl_error.xml \
	erl_eterm.xml \
	erl_format.xml \
	erl_malloc.xml \
	erl_marshal.xml \
	erl_global.xml \
	ei.xml \
	ei_connect.xml \
	registry.xml 

BOOK_FILES = book.xml
XML_APPLICATION_FILES = ref_man.xml ref_man_ei.xml ref_man_erl_interface.xml
XML_PART_FILES = \
	part_ei.xml \
	part_notes.xml \
	part_notes_history.xml
XML_CHAPTER_FILES = ei_users_guide.xml notes.xml notes_history.xml

GIF_FILES = \
	note.gif \
	warning.gif

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info
EXTRA_FILES = \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(XML_REF1_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

MAN1_FILES = $(XML_REF1_FILES:%.xml=$(MAN1DIR)/%.1)
MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.xml=%.tex)
TEX_FILES_REF_MAN = $(XML_REF1_FILES:%.xml=%.tex) \
	$(XML_REF3_FILES:%.xml=%.tex) \
	$(XML_APPLICATION_FILES:%.xml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(XML_CHAPTER_FILES:%.xml=%.tex)

TOP_PDF_FILE = erl_interface-$(VSN).pdf
TOP_PS_FILE  = erl_interface-$(VSN).ps

$(TOP_PDF_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | $(DISTILL) $(DISTILL_FLAGS) > $@

$(TOP_PS_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< > $@

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

ifeq ($(DOCTYPE),pdf)
docs: pdf
else
ifeq ($(DOCTYPE),ps)
docs: ps
else
docs: html gifs man
endif
endif

pdf: $(TOP_PDF_FILE)

ps: $(TOP_PS_FILE)

html: $(HTML_FILES)

man: $(MAN1_FILES) $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

debug opt: 

clean clean_docs clean_tex:
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES) $(MAN1_FILES) $(MAN3_FILES)
	rm -f $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f errs core *~ $(LATEX_CLEAN) min_head.gif

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

ifeq ($(DOCTYPE),pdf)
release_docs_spec: pdf
	$(INSTALL_DIR) $(RELEASE_PATH)/pdf
	$(INSTALL_DATA) $(TOP_PDF_FILE) $(RELEASE_PATH)/pdf
else
ifeq ($(DOCTYPE),ps)
release_docs_spec: ps
	$(INSTALL_DIR) $(RELEASE_PATH)/ps
	$(INSTALL_DATA) $(TOP_PS_FILE) $(RELEASE_PATH)/ps
else
release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(GIF_FILES) $(EXTRA_FILES) $(HTML_FILES) \
		$(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man1
	$(INSTALL_DATA) $(MAN1_FILES) $(RELEASE_PATH)/man/man1
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3_FILES) $(RELEASE_PATH)/man/man3
endif
endif

release_spec:


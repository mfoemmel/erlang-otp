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
default: docs

include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(COMET_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/comet-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
SGML_APPLICATION_FILES = refman.sgml
SGML_REF3_FILES = comet.sgml erl_com.sgml com_gen.sgml

SGML_PART_FILES = users_guide.sgml part_notes.sgml
SGML_CHAPTER_FILES = ch_intro.sgml ch_comet.sgml ch_examples.sgml notes.sgml

BOOK_FILES = book.sgml

GIF_FILES = \
	notes.gif \
	ref_man.gif \
	user_guide.gif \
	book.gif \
	note.gif \
	warning.gif

# ----------------------------------------------------

HTML_FILES = $(SGML_APPLICATION_FILES:%.sgml=$(HTMLDIR)/%.html) \
	$(SGML_PART_FILES:%.sgml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info
EXTRA_FILES = summary.html.src \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(SGML_REF3_FILES:%.sgml=$(HTMLDIR)/%.html) \
	$(SGML_CHAPTER_FILES:%.sgml=$(HTMLDIR)/%.html)

MAN3_FILES = $(SGML_REF3_FILES:%.sgml=$(MAN3DIR)/%.3)

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.sgml=%.tex)
TEX_FILES_REF_MAN = $(SGML_REF3_FILES:%.sgml=%.tex) \
	$(SGML_APPLICATION_FILES:%.sgml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(SGML_CHAPTER_FILES:%.sgml=%.tex)

TOP_PDF_FILE = comet-$(VSN).pdf
TOP_PS_FILE  = comet-$(VSN).ps

$(TOP_PDF_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | $(DISTILL) $(DISTILL_FLAGS) > $@

$(TOP_PS_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< > $@

TOP_HTML_FILES = $(INDEX_TARGET) 

INDEX_FILE = index.html
INDEX_SRC = $(INDEX_FILE).src
INDEX_TARGET = $(DOCDIR)/$(INDEX_FILE)

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
SGML_FLAGS += 
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
docs: html gifs $(TOP_HTML_FILES) man
endif
endif

pdf: $(TOP_PDF_FILE)

ps: $(TOP_PS_FILE)

html: $(HTML_FILES)

man: $(MAN3_FILES) 

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(INDEX_TARGET): $(INDEX_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

debug opt: 

clean_tex:
	-rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)

clean:
	rm -f ../html/* $(MAN3_FILES)  $(TEX_FILES_USERS_GUIDE) sgmls_output sgmls_error
	rm -f $(TOP_HTML_FILES) $(TOP_PDF_FILE)
	rm -f errs core *~ $(LATEX_CLEAN) 

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
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3_FILES) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(TOP_HTML_FILES) \
		$(RELSYSDIR)/doc
endif
endif

release_spec:

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

include make.dep

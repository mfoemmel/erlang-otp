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
VSN=$(EDOC_VSN)

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------
include make.dep

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/edoc-$(VSN)

# ----------------------------------------------------
# Man page source directory (with .erl files)
# ----------------------------------------------------
SRC_DIR = $(ERL_TOP)/lib/edoc/src
INC_DIR = $(ERL_TOP)/lib/edoc/include

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
SGML_APPLICATION_FILES = application.sgml
SGML_REF3_FILES = \
	edoc.sgml \
	edoc_doclet.sgml \
	edoc_extract.sgml \
	edoc_layout.sgml \
	edoc_lib.sgml \
	edoc_run.sgml

SGML_PART_FILES = part.sgml part_notes.sgml
SGML_CHAPTER_FILES = chapter.sgml
SGML_NOTES_FILES = notes.sgml

BOOK_FILES = book.sgml

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = \
	$(SGML_APPLICATION_FILES:%.sgml=$(HTMLDIR)/%.html) \
	$(SGML_PART_FILES:%.sgml=$(HTMLDIR)/%.html)

EXTRA_FILES = \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(SGML_REF3_FILES:%.sgml=$(HTMLDIR)/%.html) \
	$(SGML_CHAPTER_FILES:%.sgml=$(HTMLDIR)/%.html) \
	$(SGML_NOTES_FILES:%.sgml=$(HTMLDIR)/%.html)

MAN3_FILES = $(SGML_REF3_FILES:%.sgml=$(MAN3DIR)/%.3)

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.sgml=%.tex)
TEX_FILES_REF_MAN = $(SGML_REF3_FILES:%.sgml=%.tex) \
	$(SGML_APPLICATION_FILES:%.sgml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(SGML_CHAPTER_FILES:%.sgml=%.tex)

TOP_PDF_FILE = edoc-$(VSN).pdf
TOP_PS_FILE  = edoc-$(VSN).ps

$(TOP_PDF_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | $(DISTILL) $(DISTILL_FLAGS) > $@

$(TOP_PS_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< > $@

# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
SGML_FLAGS += 
DVIPS_FLAGS += 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------
ifeq ($(DOCTYPE),pdf)
docs: pdf
else
ifeq ($(DOCTYPE),ps)
docs: ps
else
docs: html gifs man
endif
endif

$(SGML_REF3_FILES):
	docb_gen -def vsn $(EDOC_VSN) -includes $(INC_DIR) \
		$(SRC_DIR)/$(@:%.sgml=%.erl)

$(SGML_CHAPTER_FILES):
	docb_gen -chapter -def vsn $(EDOC_VSN) ../overview.edoc

pdf: sgml $(TOP_PDF_FILE)

ps: sgml $(TOP_PS_FILE)

html: sgml $(HTML_FILES)

man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

sgml: $(SGML_REF3_FILES) $(SGML_CHAPTER_FILES)

debug opt: 

clean clean_docs clean_tex:
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES) $(MAN3_FILES)
	rm -f $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f errs core *~ *sgmls_output *sgmls_errs $(LATEX_CLEAN)
	rm -f application.kwc min_head.gif part.tex
	rm -f $(SGML_REF3_FILES) $(SGML_CHAPTER_FILES) *.html

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
	$(INSTALL_DATA) $(EXTRA_FILES) $(HTML_FILES) \
		$(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3_FILES) $(RELEASE_PATH)/man/man3
endif
endif

release_spec:

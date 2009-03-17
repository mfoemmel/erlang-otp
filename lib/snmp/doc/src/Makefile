#-*-makefile-*-   ; force emacs to enter makefile-mode

# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN = $(SNMP_VSN)

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

include make.dep

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/snmp-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include files.mk


# ----------------------------------------------------

HTML_APP_FILES  = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html)
XML_PART_FILE2 = $(XML_PART_FILES) notes_history.xml
HTML_PART_FILES = \
	../html/notes_history.html \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

HTML_FILES = \
	$(HTML_APP_FILES) \
	$(HTML_PART_FILES)

XML_ERRS = $(XML_FILES:%.xml=%.latex.xmls_errs) \
            $(XML_FILES:%.xml=%.html.xmls_errs)

XML_OUTPUT = $(XML_FILES:%.xml=%.latex.xmls_output) \
              $(XML_FILES:%.xml=%.html.xmls_output)

INFO_FILE = ../../info

HTML_REF3_FILES = $(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_REF6_FILES = $(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html)
HTML_CHAP_FILES = $(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

EXTRA_FILES = summary.html.src \
        $(DEFAULT_HTML_FILES)  \
        $(HTML_REF3_FILES)     \
        $(HTML_REF6_FILES)     \
        $(HTML_CHAP_FILES)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.xml=%.tex)
TEX_FILES_REF_MAN = \
	$(XML_REF3_FILES:%.xml=%.tex) \
	$(XML_REF6_FILES:%.xml=%.tex) \
	$(XML_APPLICATION_FILES:%.xml=%.tex)
TEX_PART_FILES = $(XML_PART_FILES:%.xml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(XML_CHAPTER_FILES:%.xml=%.tex)

TOP_PDF_FILE = snmp-$(VSN).pdf
TOP_PS_FILE  = snmp-$(VSN).ps

$(TOP_PDF_FILE): book.dvi ../../vsn.mk
	@echo "building $(TOP_PDF_FILE)"
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | $(DISTILL) $(DISTILL_FLAGS) > $@

$(TOP_PS_FILE): book.dvi ../../vsn.mk
	@echo "building $(TOP_PS_FILE)"
	$(DVI2PS) $(DVIPS_FLAGS) -f $< > $@

TOP_HTML_FILES = $(INDEX_TARGET) 

GIF_TARGETS = $(GIF_FILES:%=$(HTMLDIR)/%)

INDEX_FILE   = index.html
INDEX_SRC    = $(INDEX_FILE).src
INDEX_TARGET = $(DOCDIR)/$(INDEX_FILE)


# ----------------------------------------------------
# FLAGS 
# ----------------------------------------------------
XML_FLAGS += 
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

pdf: $(TOP_PDF_FILE)

ps: $(TOP_PS_FILE)

html: $(HTML_FILES) $(TOP_HTML_FILES) gifs 

html2: gifs $(TOP_HTML_FILES) $(HTML_FILES) $(HTML_REF3_FILES) $(HTML_REF6_FILES) $(HTML_CHAP_FILES) 

man: $(MAN3_FILES) $(MAN6_FILES)

gifs: $(GIF_TARGETS)

debug opt: 

clean: clean_tex clean_html clean_man clean_gifs clean_docs

clean_docs: 
	@echo "cleaning docs:"
	rm -f $(TOP_PDF_FILE) 
	rm -f $(TOP_PS_FILE)
	rm -f core $(LATEX_CLEAN)

clean_man:
	@echo "cleaning man:"
	rm -f $(MAN3_FILES) $(MAN6_FILES)

clean_tex:
	@echo "cleaning tex:"
	rm -f $(TEX_FILES_USERS_GUIDE) 
	rm -f $(TEX_FILES_REF_MAN) 
	rm -f $(TEX_PART_FILES) 
	rm -f $(TEX_FILES_BOOK)

clean_html:
	@echo "cleaning html:"
	rm -f $(TOP_HTML_FILES)
	rm -f $(HTML_FILES)
	rm -f $(DEFAULT_HTML_FILES)
	rm -f $(HTML_REF3_FILES)
	rm -f $(HTML_REF6_FILES)
	rm -f $(HTML_CHAP_FILES)

clean_gifs: 
	@echo "cleaning gifs:"
	rm -f $(GIF_TARGETS)

$(HTMLDIR)/%.gif: %.gif			# Copy them to ../html
	$(INSTALL_DATA) $< $@

$(INDEX_TARGET): $(INDEX_SRC) ../../vsn.mk	# Create top make file 
	sed -e 's;%VSN%;$(VSN);' $< > $@	# inserting version number

$(HTML_PART_FILES): notes.xml


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
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man6
	$(INSTALL_DATA) $(MAN6_FILES) $(RELEASE_PATH)/man/man6
	$(INSTALL_DATA) $(TOP_HTML_FILES) \
		$(RELSYSDIR)/doc
endif
endif

release_spec:

info: info_xml info_html info_tex
	@echo "DVI2PS        = $(DVI2PS)"
	@echo "DVIPS_FLAGS   = $(DVIPS_FLAGS)"
	@echo ""
	@echo "DISTILL       = $(DISTILL)"
	@echo "DISTILL_FLAGS = $(DISTILL_FLAGS)"

info_xml: 
	@echo "xml files:"
	@echo "XML_REF3_FILES        = $(XML_REF3_FILES)"
	@echo "XML_REF6_FILES        = $(XML_REF6_FILES)"
	@echo "XML_PART_FILES        = $(XML_PART_FILES)"
	@echo "XML_CHAPTER_FILES     = $(XML_CHAPTER_FILES)"
	@echo "XML_APPLICATION_FILES = $(XML_APPLICATION_FILES)"
	@echo ""
	@echo "BOOK_FILES             = $(BOOK_FILES)"
	@echo ""
	@echo "XML_FILES  = $(XML_FILES)"
	@echo "XML_ERRS   = $(XML_ERRS)"
	@echo "XML_OUTPUT = $(XML_OUTPUT)"

info_html:
	@echo "html files:"
	@echo "HTMLDIR = $(HTMLDIR)"
	@echo "HTML_APP_FILES  = $(HTML_APP_FILES)"
	@echo "HTML_PART_FILES = $(HTML_PART_FILES)"
	@echo "HTML_FILES  = $(HTML_FILES)"
	@echo ""
	@echo "EXTRA_FILES = $(EXTRA_FILES)"
	@echo ""
	@echo "DEFAULT_HTML_FILES = $(DEFAULT_HTML_FILES)"
	@echo ""
	@echo "HTML_REF3_FILES = $(HTML_REF3_FILES)"
	@echo "HTML_REF6_FILES = $(HTML_REF6_FILES)"
	@echo "HTML_CHAP_FILES = $(HTML_CHAP_FILES)"

info_tex: 
	@echo "tex files:"
	@echo "TEX_FILES_USERS_GUIDE = $(TEX_FILES_USERS_GUIDE)"
	@echo "TEX_FILES_REF_MAN     = $(TEX_FILES_REF_MAN)"
	@echo "TEX_PART_FILES        = $(TEX_PART_FILES)"
	@echo "TEX_FILES_BOOK        = $(TEX_FILES_BOOK)"

include depend.mk


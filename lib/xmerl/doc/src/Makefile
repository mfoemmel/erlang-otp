#<copyright>
# <year>2004-2007</year>
# <holder>Ericsson AB, All Rights Reserved</holder>
#</copyright>
#<legalnotice>
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
# The Initial Developer of the Original Code is Ericsson AB.
#</legalnotice>
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(XMERL_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/xmerl-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------

EDOC_DIR = $(ERL_TOP)/lib/edoc
SYNTAX_TOOLS_DIR = $(ERL_TOP)/lib/syntax_tools

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XMERL_DIR = $(ERL_TOP)/lib/xmerl/src

XMERL_MODULES = \
	xmerl_scan \
	xmerl \
	xmerl_xs \
	xmerl_eventp \
	xmerl_xpath \
	xmerl_xsd


XML_APPLICATION_FILES = ref_man.xml
XMERL_XML_FILES = $(XMERL_MODULES:=.xml)

XML_REF_MAN = ref_man.xml
XML_REF3_FILES = $(XMERL_XML_FILES)

XML_PART_FILES =  \
	part.xml \
	part_notes.xml

XML_REF6_FILES =

XML_CHAPTER_FILES =  \
	xmerl_ug.xml \
	notes.xml

HTML_EXAMPLE_FILES =  \
	xmerl_examples.html \
	xmerl_xs_examples.html

HTML_STYLESHEET_FILES = \
	../stylesheet.css

BOOK_FILES = book.xml

XML_HTML_FILES = \
	notes_history.xml

EXAMPLE_FILES = people2.txt people.txt motorcycles.txt motorcycles_dtd.txt \
	new_motorcycles.txt new_motorcycles2.txt result_export.html \
	motorcycles2.txt result_xs.html motorcycles2html.erl

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = $(XML_REF_MAN:%.xml=$(HTMLDIR)/%.html) \
	$(XML_HTML_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)


EXTRA_FILES = \
	$(DEFAULT_HTML_FILES) \
	$(DEFAULT_GIF_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.xml=%.tex)
TEX_FILES_REF_MAN = $(XML_REF3_FILES:%.xml=%.tex) \
	$(XML_REF6_FILES:%.xml=%.tex) \
	$(XML_REF_MAN:%.xml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(XML_CHAPTER_FILES:%.xml=%.tex)

TOP_PDF_FILE = xmerl-$(VSN).pdf
TOP_PS_FILE  = xmerl-$(VSN).ps

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
ifeq ($(DOCTYPE),pdf)
docs: pdf
else
ifeq ($(DOCTYPE),ps)
docs: ps
else
docs: html gifs man
endif
endif


$(XMERL_XML_FILES): 
	docb_gen $(XMERL_DIR)/$(@:%.xml=%.erl)


info:
	@echo "XML_PART_FILES: $(XML_PART_FILES)"
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "XMERL_XML_FILES: $(XMERL_XML_FILES)"
	@echo "XMERL_MODULES: $(XMERL_MODULES)"
	@echo "HTML_FILES: $(HTML_FILES)"
	@echo "HTMLDIR: $(HTMLDIR)"
	@echo "DEFAULT_GIF_FILES: $(DEFAULT_GIF_FILES)"
	@echo "DEFAULT_HTML_FILES: $(DEFAULT_HTML_FILES)"
	@echo "EXTRA_FILES: $(EXTRA_FILES)"

pdf: xml $(TOP_PDF_FILE)

ps: xml $(TOP_PS_FILE)

html: xml $(HTML_FILES)

man: $(MAN3_FILES) $(MAN6_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

xml: $(XMERL_XML_FILES)

debug opt: 


clean clean_docs clean_tex:
	rm -f $(XMERL_XML_FILES)
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES) $(MAN3_FILES) $(MAN6_FILES)
	rm -f $(EXTRA_FILES:fascicules.xml= )
	rm -f $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f errs core *~ *sgmls_output *sgmls_errs $(LATEX_CLEAN) 


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
	$(HTML_EXAMPLE_FILES) $(HTML_STYLESHEET_FILES) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(MAN3_FILES) $(RELEASE_PATH)/man/man3
	$(INSTALL_DATA) $(EXAMPLE_FILES) $(RELSYSDIR)/doc/html
endif
endif

release_spec:



# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------
-include make.dep



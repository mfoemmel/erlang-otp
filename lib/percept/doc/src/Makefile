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
VSN=$(PERCEPT_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/percept-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------

EDOC_DIR = $(ERL_TOP)/lib/edoc

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
PERCEPT_DIR = $(ERL_TOP)/lib/percept/src
RUNTIME_TOOLS_DIR = $(ERL_TOP)/lib/runtime_tools/src

PERCEPT_MODULES = \
	egd\
	percept

RUNTIME_TOOLS_MODULES = \
	percept_profile

XML_APPLICATION_FILES = \
	ref_man.xml

PERCEPT_XML_FILES = $(PERCEPT_MODULES:=.xml) 

RUNTIME_TOOLS_XML_FILES = $(RUNTIME_TOOLS_MODULES:=.xml)

MODULE_XML_FILES = $(PERCEPT_XML_FILES) $(RUNTIME_TOOLS_XML_FILES)

XML_REF_MAN = \
	ref_man.xml

XML_REF3_FILES = $(MODULE_XML_FILES)

XML_PART_FILES = \
	part.xml \
	part_notes.xml

XML_REF6_FILES =

XML_CHAPTER_FILES = \
	notes.xml \
	egd_ug.xml \
	percept_ug.xml


HTML_EXAMPLE_FILES = \
	percept_examples.html
	
HTML_STYLESHEET_FILES = \
	../stylesheet.css

BOOK_FILES = book.xml

GIF_FILES = \
	test1.gif \
	test2.gif \
	test3.gif \
	test4.gif \
	percept_overview.gif \
	percept_processes.gif \
	percept_processinfo.gif \
	percept_compare.gif \
	img_esi_result.gif 

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = \
	$(XML_REF_MAN:%.xml=$(HTMLDIR)/%.html) \
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

TOP_PDF_FILE = percept-$(VSN).pdf
TOP_PS_FILE  = percept-$(VSN).ps

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

$(HTMLDIR)/%.gif: %.gif
	$(INSTALL_DATA) $< $@

$(PERCEPT_XML_FILES):
	docb_gen $(PERCEPT_DIR)/$(@:%.xml=%.erl)

$(RUNTIME_TOOLS_XML_FILES):
	docb_gen $(RUNTIME_TOOLS_DIR)/$(@:%.xml=%.erl)

info:
	@echo "XML_PART_FILES: $(XML_PART_FILES)"
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "PERCEPT_XML_FILES: $(MODULE_XML_FILES)"
	@echo "PERCEPT_MODULES: $(PERCEPT_MODULES)"
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

xml: $(MODULE_XML_FILES)

debug opt: 


clean clean_docs clean_tex:
	rm -f $(MODULE_XML_FILES)
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES) $(MAN3_FILES) $(MAN6_FILES)
	rm -f $(EXTRA_FILES:fascicules.xml= )
	rm -f $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f min_head.gif
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
endif
endif

release_spec:



# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------
-include make.dep



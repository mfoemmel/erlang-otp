#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
#

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../../vsn.mk
VSN=$(COMMON_TEST_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/common_test-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

# REMEMBER: links to HTML files for these modules in ref_man.xml
CT_MODULES = \
	ct \
	ct_master \
	ct_cover \
	ct_telnet \
	ct_ftp \
	ct_ssh \
	ct_rpc \
	ct_snmp \
	unix_telnet

CT_XML_FILES = $(CT_MODULES:=.xml)

XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = $(CT_XML_FILES)
XML_REF6_FILES = common_test_app.xml

XML_PART_FILES = part.xml \
	part_notes.xml \
	part_notes_history.xml

XML_CHAPTER_FILES = \
	basics_chapter.xml \
	install_chapter.xml \
	write_test_chapter.xml \
	test_structure_chapter.xml \
	run_test_chapter.xml \
	example_chapter.xml \
	why_test_chapter.xml \
	config_file_chapter.xml \
	cover_chapter.xml \
	ct_master_chapter.xml \
	event_handler_chapter.xml \
	dependencies_chapter.xml \
	notes.xml \
	notes_history.xml 

XML_REF_FILES = run_test.xml

MAKE_EDOC = make_edoc

BOOK_FILES = book.xml

GIF_FILES =

INSTALL_NOTES = ../../notes.html

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)

INFO_FILE = ../../info
EXTRA_FILES = \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_REF_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.xml=%.tex)
TEX_FILES_REF_MAN = $(XML_REF3_FILES:%.xml=%.tex) \
	$(XML_REF6_FILES:%.xml=%.tex) \
	$(XML_APPLICATION_FILES:%.xml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(XML_CHAPTER_FILES:%.xml=%.tex) \
	$(XML_PART_FILES:%.xml=%.tex)

TOP_PDF_FILE = common_test-$(VSN).pdf
TOP_PS_FILE  = common_test-$(VSN).ps

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

CT_SRC_DIR = $(ERL_TOP)/../internal_tools/common_test/src

EDOC_ARGS=[{preprocess,true},{includes,["$(XMERL_DIR)/include","../../../test_server/include","../../include","../../../../erts/lib/kernel/include","../../../../lib/kernel/include","../../../../erts/lib/snmp/include","../../../../lib/snmp/include"]}]

$(CT_XML_FILES): 
	erl -boot start_clean -noshell \
	-eval 'docb_gen:module("../../src/$(@:%.xml=%.erl)",$(EDOC_ARGS))' \
	-s erlang halt

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

pdf: xml $(TOP_PDF_FILE)

ps: xml $(TOP_PS_FILE)

html: xml $(HTML_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

xml: $(CT_XML_FILES) 

man: $(MAN6_FILES) $(MAN3_FILES)

debug opt: 

#
# checkout make.dep before generating new dependecies
#
make_doc_depend: xml
	docdepend > make.dep

clean clean_docs clean_tex:
	rm -f $(CT_XML_FILES)
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES)
	rm -f $(MAN3_FILES) 
	rm -f $(MAN6_FILES)
	rm -f $(GIF_FILES:%=$(HTMLDIR)/%)
	rm -f $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f errs core *~ *xmls_output *xmls_errs $(LATEX_CLEAN) 

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
#	$(INSTALL_DATA) $(INSTALL_NOTES) $(RELSYSDIR)
endif
endif

release_spec:

release_tests_spec:

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

include make.dep
# DO NOT DELETE



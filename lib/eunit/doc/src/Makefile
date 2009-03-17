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
VSN=$(EUNIT_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/eunit-$(VSN)

# ----------------------------------------------------
# Help application directory specification
# ----------------------------------------------------

EDOC_DIR = $(ERL_TOP)/lib/edoc
SYNTAX_TOOLS_DIR = $(ERL_TOP)/lib/syntax_tools

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
EUNIT_DIR = $(ERL_TOP)/lib/eunit/src
EUNIT_INC_DIR = $(ERL_TOP)/lib/eunit/include

EUNIT_MODULES = \
	eunit

XML_APPLICATION_FILES = ref_man.xml

XML_REF3_FILES =  $(EUNIT_MODULES:=.xml) 

XML_PART_FILES =  \
	part.xml \
	part_notes.xml

XML_CHAPTER_FILES =  \
	chapter.xml

XML_NOTES_FILES =  \
	notes.xml

HTML_EXAMPLE_FILES =  

HTML_STYLESHEET_FILES = \
	../stylesheet.css

BOOK_FILES = book.xml

# ----------------------------------------------------
INFO_FILE = ../../info

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)


EXTRA_FILES = \
	$(DEFAULT_HTML_FILES) \
	$(DEFAULT_GIF_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)\
	$(XML_NOTES_FILES:%.xml=$(HTMLDIR)/%.html)

MAN3_FILES = $(XML_REF3_FILES:%.xml=$(MAN3DIR)/%.3)
MAN6_FILES = $(XML_REF6_FILES:%_app.xml=$(MAN6DIR)/%.6)

TEX_FILES_BOOK = \
	$(BOOK_FILES:%.xml=%.tex)
TEX_FILES_REF_MAN = $(XML_REF3_FILES:%.xml=%.tex) \
	$(XML_REF6_FILES:%.xml=%.tex) \
	$(XML_APPLICATION_FILES:%.xml=%.tex)
TEX_FILES_USERS_GUIDE = \
	$(XML_CHAPTER_FILES:%.xml=%.tex)

TOP_PDF_FILE = eunit-$(VSN).pdf
TOP_PS_FILE  = eunit-$(VSN).ps

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


$(XML_REF3_FILES): 
	docb_gen -def vsn $(EUNIT_VSN) -includes $(EUNIT_INC_DIR) $(EUNIT_DIR)/$(@:%.xml=%.erl)

$(XML_CHAPTER_FILES):
	docb_gen -chapter -def vsn $(EUNIT_VSN) ../overview.edoc


info:
	@echo "XML_PART_FILES: $(XML_PART_FILES)"
	@echo "XML_APPLICATION_FILES: $(XML_APPLICATION_FILES)"
	@echo "EUNIT_XML_FILES: $(EUNIT_XML_FILES)"
	@echo "EUNIT_MODULES: $(EUNIT_MODULES)"
	@echo "HTML_FILES: $(HTML_FILES)"
	@echo "HTMLDIR: $(HTMLDIR)"
	@echo "DEFAULT_GIF_FILES: $(DEFAULT_GIF_FILES)"
	@echo "DEFAULT_HTML_FILES: $(DEFAULT_HTML_FILES)"
	@echo "EXTRA_FILES: $(EXTRA_FILES)"

pdf: xml $(TOP_PDF_FILE)

ps: xml $(TOP_PS_FILE)

html: xml $(HTML_FILES)

man: $(MAN3_FILES)

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

xml: $(XML_REF3_FILES) $(XML_CHAPTER_FILES)

debug opt: 


clean clean_docs clean_tex:
	rm -f $(XML_REF3_FILES) $(XML_CHAPTER_FILES) *.html
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES) $(MAN3_FILES) 
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

endif
endif

release_spec:



# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------
-include make.dep



# -*-Makefile-*-

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
VSN=$(JINTERFACE_VSN)

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

include make.dep


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/jinterface-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

# Renamed this var to fool otp.mk.in
XML_APP_FILES = ref_man.xml
XML_REF3_FILES = jinterface.xml

XML_PART_FILES = \
	part.xml \
	part_notes.xml \
	part_notes_history.xml

XML_CHAPTER_FILES = \
	notes.xml \
	notes_history.xml \
	jinterface_users_guide.xml

BOOK_FILES = book.xml

GIF_FILES = \
	notes.gif \
	ref_man.gif \
	user_guide.gif

#------------------------------------------------------


TEX_FILES_BOOK = \
	$(BOOK_FILES:%.xml=%.tex)
TEX_FILES_REF_MAN = $(XML_REF3_FILES:%.xml=%.tex) \
	$(XML_APP_FILES:%.xml=%.tex)
TEX_FILES_USERS_GUIDE = \
	part.tex $(XML_CHAPTER_FILES:%.xml=%.tex)


TOP_PDF_FILE = jinterface-$(VSN).pdf
TOP_PS_FILE  = jinterface-$(VSN).ps

$(TOP_PDF_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< | $(DISTILL) $(DISTILL_FLAGS) > $@

$(TOP_PS_FILE): book.dvi ../../vsn.mk
	$(DVI2PS) $(DVIPS_FLAGS) -f $< > $@


JAVADOC = javadoc
JAVADOC_PKGS = com.ericsson.otp.erlang
JAVA_PKG_PATH = com/ericsson/otp/erlang
JAVADOC_TITLE = 'Java-Erlang Interface Library'
JAVA_TOP_INDEX = $(HTMLDIR)/java/index.html
JAVADOC_DEST = ../doc/html/java
JAVA_SRC_PATH = $(ERL_TOP)/lib/jinterface/java_src/$(JAVA_PKG_PATH)

# don't add filenames to the Makefile!
# all java sourcefiles listed in common include file
include $(JAVA_SRC_PATH)/java_files

JAVA_SRC_FILES = $(JAVA_FILES:%=$(JAVA_SRC_PATH)/%.java)

JAVA_DOC_FILES = \
	overview-tree.html \
	index-all.html \
	deprecated-list.html \
	allclasses-frame.html \
	packages.html \
	serialized-form.html \
	package-list \
	stylesheet.css \
	help-doc.html 

INFO_FILE = ../../info
JAVA_EXTRA_FILES = $(JAVA_DOC_FILES:%=$(HTMLDIR)/java/%)

JAVA_GEN_FILES = \
	$(JAVA_FILES:%=$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/%.html) \
	$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/package-summary.html \
	$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/package-tree.html \
	$(JAVADOC_DEST)/$(JAVA_PKG_PATH)/package-frame.html


# ----------------------------------------------------


HTML_FILES = \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html)


EXTRA_FILES = \
	summary.html.src \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(XML_CHAPTER_FILES:%.xml=$(HTMLDIR)/%.html)



TOP_HTML_FILES = $(INDEX_TARGET) 

INDEX_FILE = index.html
INDEX_SRC = $(INDEX_FILE).src
INDEX_TARGET = $(DOCDIR)/$(INDEX_FILE)

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
docs: html gifs $(TOP_HTML_FILES) jdoc man
endif
endif

pdf: $(TOP_PDF_FILE)

ps: $(TOP_PS_FILE)

html:$(HTML_FILES)

jdoc:$(JAVA_SRC_FILES)
	(cd ../../java_src;$(JAVADOC) -sourcepath . -d $(JAVADOC_DEST) \
		-windowtitle $(JAVADOC_TITLE) $(JAVADOC_PKGS))

man: 

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(INDEX_TARGET): $(INDEX_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

debug opt: 

clean clean_docs clean_tex:
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES) $(MAN3_FILES)
	rm -f $(TOP_HTML_FILES) $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f errs core *~ *dump min_head.gif $(LATEX_CLEAN)
	rm -f $(JAVA_GEN_FILES)


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
	$(INSTALL_DATA) $(INFO_FILE) $(RELSYSDIR)
	$(INSTALL_DATA) $(GIF_FILES) $(EXTRA_FILES) $(HTML_FILES) \
		$(RELSYSDIR)/doc/html
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html/java/$(JAVA_PKG_PATH)
	$(INSTALL_DATA) $(JAVA_EXTRA_FILES) $(JAVA_TOP_INDEX) \
		$(RELSYSDIR)/doc/html/java
	(/bin/cp -rf  ../../doc/html $(RELSYSDIR)/doc)
	$(INSTALL_DATA) $(TOP_HTML_FILES) $(RELSYSDIR)/doc
endif
endif

release_spec:

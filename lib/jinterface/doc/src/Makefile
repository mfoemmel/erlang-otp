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
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/jinterface-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------


SGML_PART_FILES = part_notes.sgml
SGML_CHAPTER_FILES = notes.sgml

GIF_FILES = \
	notes.gif \
	ref_man.gif

JAVADOC = javadoc
JAVADOC_PKGS = com.ericsson.otp.erlang
JAVA_PKG_PATH = com/ericsson/otp/erlang
JAVADOC_TITLE = 'Java-Erlang Interface Library'
JAVA_TOP_INDEX = $(HTMLDIR)/index.html
JAVADOC_DEST = ../doc/html


JAVA_FILES = \
	OtpConnection \
	OtpEpmd \
	OtpErlangAtom \
	OtpErlangBinary \
	OtpErlangBoolean \
	OtpErlangByte \
	OtpErlangChar \
	OtpErlangDouble \
	OtpErlangFloat \
	OtpErlangInt \
	OtpErlangList \
	OtpErlangLong \
	OtpErlangObject \
	OtpErlangPid \
	OtpErlangPort \
	OtpErlangRef \
	OtpErlangShort \
	OtpErlangString \
	OtpErlangTuple \
	OtpErlangUInt \
	OtpErlangUShort \
	OtpExternal \
	OtpInputStream \
	OtpNode \
	OtpOutputStream \
	OtpPeer \
	OtpSelf \
	OtpServer \
	OtpAuthException \
	OtpErlangDataException \
	OtpErlangDecodeException \
	OtpErlangException \
	OtpErlangExit \
	OtpErlangRangeException \
	OtpException 

JAVA_SRC_FILES = $(JAVA_FILES:%=../../java_src/$(JAVA_PKG_PATH)/%.java)

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

JAVA_EXTRA_FILES = $(JAVA_DOC_FILES:%=$(HTMLDIR)/%)

JAVA_GEN_FILES = \
	$(JAVA_FILES:%=$(HTMLDIR)/$(JAVA_PKG_PATH)/%.html) \
	$(HTMLDIR)/$(JAVA_PKG_PATH)/package-summary.html \
	$(HTMLDIR)/$(JAVA_PKG_PATH)/package-tree.html \
	$(HTMLDIR)/$(JAVA_PKG_PATH)/package-frame.html
	

# ----------------------------------------------------


HTML_FILES = $(SGML_PART_FILES:%.sgml=$(HTMLDIR)/%.html)

EXTRA_FILES = summary.html.src \
	../html/min_head.gif \
	$(DEFAULT_HTML_FILES) \
	$(SGML_CHAPTER_FILES:%.sgml=$(HTMLDIR)/%.html)



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
docs: html gifs $(TOP_HTML_FILES) javadoc man
endif
endif

pdf: 

ps: 

html: $(HTML_FILES)

javadoc:	$(JAVA_TOP_INDEX)
	
$(JAVA_TOP_INDEX):	$(JAVA_SRC_FILES)
	(cd ../../java_src; $(JAVADOC) -d $(JAVADOC_DEST) -windowtitle $(JAVADOC_TITLE) $(JAVADOC_PKGS))
	
man: 

gifs: $(GIF_FILES:%=$(HTMLDIR)/%)

$(INDEX_TARGET): $(INDEX_SRC) ../../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

debug opt: 

clean_tex:

clean:
	rm -f ../html/*  $(JAVA_GEN_FILES)
	rm -f $(TOP_HTML_FILES)
	rm -f  core *~  

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

ifeq ($(DOCTYPE),pdf)
release_docs_spec: pdf
else
ifeq ($(DOCTYPE),ps)
release_docs_spec: ps
else
release_docs_spec: docs
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html
	$(INSTALL_DATA) $(GIF_FILES) $(EXTRA_FILES) $(JAVA_EXTRA_FILES) $(JAVA_TOP_INDEX) \
		$(HTML_FILES) $(RELSYSDIR)/doc/html
	$(INSTALL_DIR) $(RELSYSDIR)/doc/html/$(JAVA_PKG_PATH)
	$(INSTALL_DATA) $(JAVA_GEN_FILES) $(RELSYSDIR)/doc/html/$(JAVA_PKG_PATH)
	$(INSTALL_DATA) $(TOP_HTML_FILES) \
		$(RELSYSDIR)/doc
endif
endif

release_spec:

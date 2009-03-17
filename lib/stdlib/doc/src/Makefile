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
VSN=$(STDLIB_VSN)

# ----------------------------------------------------
# Include dependency
# ----------------------------------------------------

include make.dep

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/stdlib-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
XML_APPLICATION_FILES = ref_man.xml
XML_REF3_FILES = \
	array.xml \
	base64.xml \
	beam_lib.xml \
	c.xml \
	calendar.xml \
	dets.xml \
	dict.xml \
	digraph.xml \
	digraph_utils.xml \
	epp.xml \
	erl_eval.xml \
	erl_expand_records.xml \
	erl_id_trans.xml \
	erl_internal.xml \
	erl_lint.xml \
	erl_parse.xml \
	erl_pp.xml \
	erl_scan.xml \
	erl_tar.xml \
	ets.xml \
	file_sorter.xml \
	filelib.xml \
	filename.xml \
	gb_sets.xml \
	gb_trees.xml \
	gen_event.xml \
	gen_fsm.xml \
	gen_server.xml \
	io.xml \
	io_lib.xml \
	lib.xml \
	lists.xml \
	log_mf_h.xml \
	math.xml \
	ms_transform.xml \
	orddict.xml \
	ordsets.xml \
	pg.xml \
	pool.xml \
	proc_lib.xml \
	proplists.xml \
	qlc.xml \
	queue.xml \
	random.xml \
	re.xml \
	regexp.xml \
	sets.xml \
	shell.xml \
	shell_default.xml \
	slave.xml \
	sofs.xml \
	string.xml \
	supervisor.xml \
	supervisor_bridge.xml \
	sys.xml \
	timer.xml \
	unicode.xml \
	win32reg.xml \
	zip.xml

XML_REF6_FILES = stdlib_app.xml

XML_PART_FILES = part.xml part_notes.xml part_notes_history.xml
XML_CHAPTER_FILES = io_protocol.xml unicode_usage.xml notes.xml notes_history.xml
GIF_FILES = ushell1.gif ushell2.gif ushell3.gif

BOOK_FILES = book.xml

# ----------------------------------------------------

HTML_FILES = $(XML_APPLICATION_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_PART_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(GIF_FILES:%.gif=$(HTMLDIR)/%.gif)

INFO_FILE = ../../info
EXTRA_FILES = \
	$(DEFAULT_GIF_FILES) \
	$(DEFAULT_HTML_FILES) \
	$(XML_REF3_FILES:%.xml=$(HTMLDIR)/%.html) \
	$(XML_REF6_FILES:%.xml=$(HTMLDIR)/%.html) \
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
	part.tex

TOP_PDF_FILE = stdlib-$(VSN).pdf
TOP_PS_FILE  = stdlib-$(VSN).ps

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
docs: html man
endif
endif

pdf: $(TOP_PDF_FILE)

ps: $(TOP_PS_FILE)

html: $(HTML_FILES)

man: $(MAN3_FILES) $(MAN6_FILES)

debug opt: 

clean clean_docs clean_tex:
	rm -f $(TEX_FILES_USERS_GUIDE) $(TEX_FILES_REF_MAN) $(TEX_FILES_BOOK)
	rm -f $(HTML_FILES) $(MAN3_FILES) $(MAN6_FILES)
	rm -f $(TOP_PDF_FILE) $(TOP_PS_FILE)
	rm -f errs core *~ min_head.gif $(LATEX_CLEAN) 

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
	$(INSTALL_DIR) $(RELEASE_PATH)/man/man6
	$(INSTALL_DATA) $(MAN6_FILES) $(RELEASE_PATH)/man/man6
endif
endif

release_spec:

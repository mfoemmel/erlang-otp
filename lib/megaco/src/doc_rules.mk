# ----------------------------------------------------
#       Documentation section
# ----------------------------------------------------
export VSN
export TEXINPUTS
export DVIPSHEADERS
export SGML_ROOT
export TEXFONTS
export TEXCONFIG
export VFFONTS

##import SGML_ROOT
ifdef SGML_DEBUG
SGML_ROOT = $(SGML_DEBUG)
endif

ifndef SGML_ROOT
SGML_ROOT = /home/otp/sgml/sgml_v3
endif


DOCDIR = ..

HTMLDIR = $(DOCDIR)/html

MAN1DIR = $(DOCDIR)/man1
MAN2DIR = $(DOCDIR)/man2
MAN3DIR = $(DOCDIR)/man3
MAN4DIR = $(DOCDIR)/man4
MAN6DIR = $(DOCDIR)/man6
MAN9DIR = $(DOCDIR)/man9

TEXDIR = .

# HTML & GIF files that always are generated and must be delivered 
SGML_COLL_FILES = $(SGML_APPLICATION_FILES) $(SGML_PART_FILES)
DEFAULT_HTML_FILES = $(SGML_COLL_FILES:%.sgml=$(HTMLDIR)/%_frame.html) \
        $(SGML_COLL_FILES:%.sgml=$(HTMLDIR)/%_first.html) \
        $(SGML_COLL_FILES:%.sgml=$(HTMLDIR)/%_term.html) \
        $(SGML_COLL_FILES:%.sgml=$(HTMLDIR)/%_cite.html) \
        $(SGML_APPLICATION_FILES:%.sgml=$(HTMLDIR)/%_index.html)

DEFAULT_GIF_FILES = $(HTMLDIR)/min_head.gif $(HTMLDIR)/note.gif $(HTMLDIR)/warning.gif

# Flags & Commands
#SGML_FLAG_term = -ptype unix -ptype windows \
#        -defs term $(ERL_TOP)/system/doc/definitions/term.defs \
#        -defs cite  $(ERL_TOP)/system/doc/definitions/cite.defs --
SGML_FLAG_term = -ptype unix -ptype windows  --
SGML_FLAG_booksty = -booksty otpA4
SGML_FLAGS = -s $(SGML_FLAG_term) $(SGML_FLAG_booksty)

SGMLTRANSFORM = sgml_transform

# LaTeX files to be rm'd when doing 'clearmake clean'
LATEX_CLEAN = \
        *.aux \
        *.cites \
        *.citeshd \
        *.dvi \
        *.idx \
        *.ilg \
        *.ind \
        *.indhd \
        *.lof \
        *.lofhd \
        *.log \
        *.lot \
        *.lothd \
        *.otpdef \
        *.otpuse \
        *.terms \
        *.termshd \
        *.toc \
        *.makeindexlog \
        *.dvipslog

ifndef LATEX_VERSION
    LATEX_VERSION = latex2e
endif

OTPTEX_ROOT = $(SGML_ROOT)/tex
OTPTEXINPUTS = $(OTPTEX_ROOT)/inputs


TEXFONTS209 = /usr/local/share/lib/tex/local/fonts:
DVIPS_FLAGS209 = -Z -t a4 -D 600 -h fontunivershack.header

TEXFONTS = $(OTPTEX_ROOT)/fonts//tfm:$(TEXFONTS209)
TEXINPUTS = .:$(OTPTEXINPUTS):$(ERL_TOP)/:$(OTPTEX_ROOT)/fonts//dvips:$(OTPTEX_ROOT)/fonts//tex:
DVIPSHEADERS = .:$(OTPTEXINPUTS):$(OTPTEX_ROOT)/fonts//dvips:
TEXCONFIG = $(OTPTEX_ROOT)/fonts:$(OTPTEX_ROOT)/fonts//dvips:
VFFONTS = $(OTPTEX_ROOT)/fonts//vf:
DVIPS_FLAGS = -Z -t a4 -D 600 -POTP


LATEX = echo | latex
LaTeX = if ( fgrep 'Label(s) may have changed.' \
                $*.log >/dev/null ) ; \
                then $(LATEX) $* ; fi
DVI2PS = dvips

# Rules
$(HTMLDIR)/%.html: %.sgml
	$(SGMLTRANSFORM) -html -outdir $(HTMLDIR) $(SGML_FLAGS) $*
	@$(RM) -f $*.html.sgmls_errs $*.html.sgmls_output

$(MAN1DIR)/%.1: %.sgml
	$(SGMLTRANSFORM) -m 1 -outdir $(MAN1DIR) $(SGML_FLAGS) $*
	@$(RM) -f $*.man1.sgmls_errs $*.man1.sgmls_output

$(MAN2DIR)/%.2: %.sgml
	$(SGMLTRANSFORM) -m 2 -outdir $(MAN2DIR) $(SGML_FLAGS) $*
	@$(RM) -f $*.man2.sgmls_errs $*.man2.sgmls_output

$(MAN3DIR)/%.3: %.sgml
	$(SGMLTRANSFORM) -m 3 -outdir $(MAN3DIR) $(SGML_FLAGS) $*
	@$(RM) -f $*.man3.sgmls_errs $*.man3.sgmls_output

$(MAN4DIR)/%.4: %.sgml
	$(SGMLTRANSFORM) -m 4 -outdir $(MAN4DIR) $(SGML_FLAGS) $*
	@$(RM) -f $*.man4.sgmls_errs $*.man4.sgmls_output

$(MAN6DIR)/%.6: %_app.sgml
	$(SGMLTRANSFORM) -m 6 -outdir $(MAN6DIR) $(SGML_FLAGS) $*_app
	@$(RM) -f $*_app.man6.sgmls_errs $*_app.man6.sgmls_output
	mv $(MAN6DIR)/$*_app.6 $@

$(MAN9DIR)/%.9: %.sgml
	$(SGMLTRANSFORM) -m 9 -outdir $(MAN9DIR) $(SGML_FLAGS) $*
	@$(RM) -f $*.man9.sgmls_errs $*.man9.sgmls_output


.sgml.html:
	$(SGMLTRANSFORM) -html $(SGML_FLAGS) $*
	@$(RM) -f $*.html.sgmls_errs $*.html.sgmls_output

.sgml.tex:
	(cd `dirname $*`; $(SGMLTRANSFORM)  -includepath `pwd` \
	        -latex $(SGML_FLAGS) `basename $*`; \
	        $(RM) -f sgmls_errs sgmls_output)

.sgml.1:
	$(SGMLTRANSFORM) -m 1 $(SGML_FLAGS) $*
	@$(RM) -f $*.man1.sgmls_errs $*.man1.sgmls_output

.sgml.2:
	$(SGMLTRANSFORM) -m 2 $(SGML_FLAGS) $*
	@$(RM) -f $*.man2.sgmls_errs $*.man2.sgmls_output

.sgml.3:
	$(SGMLTRANSFORM) -m 3 $(SGML_FLAGS) $*
	@$(RM) -f $*.man3.sgmls_errs $*.man3.sgmls_output

.sgml.4:
	$(SGMLTRANSFORM) -m 4 $(SGML_FLAGS) $*
	@$(RM) -f $*.man4.sgmls_errs $*.man4.sgmls_output

.sgml.6:
	$(SGMLTRANSFORM) -m 6 $(SGML_FLAGS) $*
	@$(RM) -f $*.man6.sgmls_errs $*.man6.sgmls_output

.sgml.9:
	$(SGMLTRANSFORM) -m 9 $(SGML_FLAGS) $*
	@$(RM) -f $*.man9.sgmls_errs $*.man9.sgmls_output



.tex.dvi:
	@echo LATEX et al on $*.tex to make $*.dvi Logs on $*.log and $*.makeindexlog
	@$(LATEX) $* > /dev/null

# 	Make term definitions.
	@if [ -f $*.otpdef -a -f $*.otpuse ] ;\
	then \
	    grep '^[ ]*term[ ]*@' $*.otpdef | sort -t@ +0 -2u -o $*.tmpdef ;\
	    grep '^[ ]*term[ ]*@' $*.otpuse | sort -t@ +1 -2u | \
	    join -j 2 -o 1.1 2.3 2.6 2.4 2.5 -t@ - $*.tmpdef | \
	    sort -t@ +1 -2f | \
	    awk -F@ '{print "\\aterm{" $$2 "}{" $$3 "}{" $$4 "}"}' > $*.terms;\
	    \
	    grep '^[ ]*cite[ ]*@' $*.otpdef | sort -t@ +0 -2uf -o $*.tmpdef ;\
	    grep '^[ ]*cite[ ]*@' $*.otpuse | sort -t@ +1 -2uf | \
	    join -j 2 -o 1.1 2.3 2.6 2.4 2.5 -t@ - $*.tmpdef | \
	    awk -F@ '{print "\\acite{" $$2 "}{" $$3 "}{" $$4 "}"}' > $*.cites;\
	    \
	    rm -f $*.tmpdef ;\
	fi

#	Make headings for some lists
	@if [ -s $*.terms ] ; then echo "\otptermshead" > $*.termshd ; fi
	@if [ -s $*.cites ] ; then echo "\otpciteshead" > $*.citeshd ; fi
	@if ( `grep contentsline $*.lot >/dev/null` ) ; \
	then \
	    echo "\otplothead" > $*.lothd ; \
	fi
	@if ( `grep contentsline $*.lof >/dev/null` ) ; \
	then \
	    echo "\otplofhead" > $*.lofhd ; \
	fi

	@$(LaTeX) > /dev/null
	@echo This will take some more time
#	Make index
	@if [ -f $*.idx ] ; \
	then\
	    makeindex -s $(OTPTEXINPUTS)/indexstyle $* 1>$*.makeindexlog 2>&1;\
	    if ( `egrep '(rejected|warnings)' $*.makeindexlog | \
				egrep -v ' 0 (rejected|warnings)'` ) ; then echo -n ; fi ; \
	fi
	@if [ -s $*.ind ] ; then echo "\otpindexhead" > $*.indhd ; fi
	@$(LATEX) $* > /dev/null
	@$(LaTeX) > /dev/null
	@if ( egrep -i '(warning|error)' $*.log ) ; then echo -n ; fi

#.dvi.ps:
#	@if [ "$(shell type $(DVI2PS))" = "dvips is /usr/local/bin/dvips" ] ; \
#	then \
#	    echo $(DVI2PS) $(DVIPS_FLAGS209) -o $@ $< ; \
#	    $(DVI2PS) $(DVIPS_FLAGS209) -o $@ $< ; \
#	else \
#	    echo $(DVI2PS) $(DVIPS_FLAGS) -o $@ $< ; \
#	    $(DVI2PS) $(DVIPS_FLAGS) -o $@ $< ; \
#	fi  2> $<.dvipslog 

#.ps.pdf:
#	$(DISTILL) $(DISTILL_FLAGS) < $< > $@

#.fig.ps:
#	@fig2dev -L ps -p a $< $@

